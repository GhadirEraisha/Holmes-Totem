package org.novetta.zoo.actors

import java.io.{File, FileOutputStream}
import java.util.concurrent.{ExecutorService, Executors}

import akka.actor._
import com.ning.http.client.{AsyncHttpClient, AsyncHttpClientConfig}
import dispatch.{as, url, _}
import org.joda.time.{DateTime, Duration}
import org.novetta.zoo.types._
import org.novetta.zoo.util.{DownloadMethods, MonitoredActor}

import scala.concurrent.Future
import scala.language.postfixOps

trait DownloadStatus
case class FailedDownload() extends DownloadStatus
case class SuccessfulDownload(filepath: String, MD5Hash: String, SHA1Hash: String, SHA256Hash: String) extends DownloadStatus

/**
 * @constructor This is the companion object to the class. Simplifies Props() nonsense.
 */
object WorkActor {
  def props(deliverytag: Long, filename: String, hashfilename: String, primaryURI: String, secondaryURI: String, WorkToDo: List[TaskedWork], attempts: Int): Props = {
    Props(new WorkActor(deliverytag, filename, hashfilename, primaryURI, secondaryURI, WorkToDo, attempts) )
  }}
/**
 * This actor represents the state of a message and its associated work within the system. As ScalaDoc's support for match
 * statement documentation is unsatisfying, the accepted messages types will be documented here.
 *
 * As this actor is effectively a stateholder, it is relatively fat. All knowledge about completed work, downloaded files, and
 * overall completion is held here. Actors terminate themselves at the completion of their work, or at the end of the timeout
 * cycle (currently 180 seconds). During termination, all completed work is sent to RMQ for storage into the cluster, and
 * incomplete work is requeued into the RMQ work queue for another attempt.
 *
 * This actor, after being created, immediately queues a download attempt with the Downloader actor.
 *
 * Something like:
 * {{{
 *   val myWorker: ActorRef = context.watch(context.actorOf(WorkActor.props(channel, key, value.filename, primaryURI, secondaryURI, value.workToDo), key.toString))
 * }}}
 * is the preferred way to create this actor.
 *
 * The following is a listing of the message types that this Actor explicitly handles, and a brief discussion of their purpose.
 * {{{
 *   case WorkComplete(work: TaskedWork, result: Option[WorkResult]) => {
 *     Each worker responds with a WorkComplete message after operating on some TaskedWork. These results are compiled,
 *     stored, and when either a timeout or full completion occurs, results are pushed to RMQ.
 *   }
 *   case FailedDownload(key: Long, attempts: Int) => {
 *     In the event that a download fails, the actor is notified, and attempts to download again (to a max of 3 times). Should
 *     the download fail entirely, Evict.
 *   }
 *   case qw: QueueWork => {
 *     Send the TaskedWork, as represented by workToDo, to the workers.
 *   }
 *   case Evict(key: Long, reason: String) => {
 *     When a death condition has been reached, send this message to self. Upon consumption, send whatever is needed to RMQ,
 *     Ack message, consume PoisionPill, and terminate.
 *   }
 *
 * }}}
 * @constructor Create a new WorkActor which holds and manages state for each message (ZooWork) that the consumer receives.
 * @param deliverytag: a Long, represents the message's ID from RMQ.
 * @param filename: a String, the name of the file we are downloading.
 * @param primaryURI: a String, the first URI we will try to use for a download.
 * @param secondaryURI:  a String, the second URI we will try to use for a download.
 * @param workToDo: a List[TaskedWork], which represents all the work we need to do for this message
 *
 */

class WorkActor(deliverytag: Long, filename: String, hashfilename: String, primaryURI: String, secondaryURI: String, workToDo: List[TaskedWork], attempts: Int) extends Actor with ActorLogging with MonitoredActor {
  import context.dispatcher
  val key = deliverytag
  val created: DateTime = new DateTime()
  var standoff: Conflict = Conflict(false, false, false, false, false)
  lazy val execServ: ExecutorService = Executors.newFixedThreadPool(40)

  val pythonDispatcher = context.actorSelection("/user/pythonDispatcher")
  val producer = context.actorSelection("/user/producer")
  var results: Map[TaskedWork, Option[WorkResult]] = workToDo.map(w => w -> None).toMap
  var MD5: String = ""
  var SHA1: String = ""
  var SHA256: String = ""

  override def postStop() {
    myHttp.shutdown()
    execServ.shutdown()
  }
  val config = new AsyncHttpClientConfig.Builder()
    .setRequestTimeout( 500 ) //should have a config value for this
    .setExecutorService(execServ)
    .setAllowPoolingConnections(true)
    .setConnectTimeout( 500 )
    //.setMaxConnections(1)
    //.setMaxConnectionsPerHost(1)
    .setIOThreadMultiplier(4).build()
  lazy val client = new AsyncHttpClient(config)
  lazy val asyncHttpClient = new AsyncHttpClient(config)
  implicit lazy val myHttp = new Http(asyncHttpClient)

  val downloadResult = myHttp(url(primaryURI) OK as.Bytes)
    .option
    .map({
      case Some(v: Array[Byte]) =>
        new FileOutputStream ("/tmp/" + filename, false).write (v) //this filepath can be a conf. variable
        log.info ("Successfully downloaded {} using the primary URI", filename)

        SuccessfulDownload("/tmp/" + filename, DownloadMethods.MD5(v), DownloadMethods.SHA1(v), DownloadMethods.SHA256(v) )

      case None =>
        log.info("Could not download {} using ANY URI", filename)

        FailedDownload()

  }).foreach(self ! _)
  /**
   * Helper function to compare two JodaTime DateTimes.
   *
   * @return A Duration object representing the delta between origin and current.
   * @param origin: An Optional DateTime, done as we do not always have pre-populated times.
   * @param current: The datetime to compare to the origin.
   */
  def timeDelta(origin: Option[DateTime], current: DateTime): Duration = {
    origin match {
      case Some(dt1: DateTime) => new Duration(dt1, current)
      case None => new Duration(null, current)
    }
  }
  def prepareCompletedWork(res: List[WorkResult]): List[WorkResult] = {
    val somes = res.collect({
      case r: WorkSuccess =>
        r
    })
    somes
  }

  def prepareFailedWork(res: List[WorkResult]): ZooWork = {
    val z = ZooWork(primaryURI, secondaryURI, hashfilename, Map[String, List[String]](), attempts)
    log.info("Our input to failedwork:", res)
    val nones = res.collect({
      case i: WorkFailure =>
        log.info("we have a workfailure {}", i)
        i
    })
    log.info("failures: {}",nones)
    val f = nones.foldLeft(z)(
        (b, a) => b + a
      )
    log.info("emitted failures: {}", f)
    f
  }
  /**
   *
   * @param completionState
   * @return Boolean, representing if the standoff is resolved, and we can die
   */
  def StandoffResolved(completionState: Conflict): Boolean = {
    completionState.consumer && completionState.result && completionState.remainder && completionState.local
  }
  def NackState(completionState: Conflict): Boolean = {
    completionState.local && completionState.nack
  }
  def AckState(completionState: Conflict): Boolean = {
    completionState.local && completionState.result && completionState.remainder && !completionState.consumer
  }

  def monitoredReceive = {
    case FailedDownload() =>
      val time = timeDelta(Some(created), DateTime.now())
      log.info("Evicting for Work {} due to {}. Evict message took {} to be generated", key, "Failed Download", time)
      log.info("We failed to download the file! Nack and Die!")
      self ! LocalResolution(true)
      context.parent ! NAck(key)

    case SuccessfulDownload(filepath: String, md5sum: String, sha1sum: String, sha256sum: String) =>
      val time = timeDelta(Some(created), DateTime.now())
      log.info("Downloaded {} successfully, in {}!", md5sum, time)
      log.info("workload: {}", workToDo)
      val w = workToDo.map(k =>
        k.doWork()
      )
      val FutureResults = Future.sequence(w)
      FutureResults.foreach(li => {
        val failures = prepareFailedWork(li)
        val successes = prepareCompletedWork(li)
        log.info("successes: {}", successes)

        if (successes.nonEmpty) {
          val time = timeDelta(Some(created), DateTime.now())

          log.info("we have nonempty successes. sending {} to producer. Took {} to generate", successes, time)
          producer ! ResultPackage(hashfilename, successes, md5sum, sha1sum, sha256sum)
        } else {
          self ! ResultResolution(true)
        }
        log.info("failures: {}", failures)
        if (failures.tasks.nonEmpty) {
          val time = timeDelta(Some(created), DateTime.now())
          log.info("we have nonempty failures. sending {} to producer. Took {} to generate", failures, time)

          producer ! failures
        } else {
          self ! RemainderResolution(true)
        }
      })
      log.info("completing local gunslinger")
      self ! LocalResolution(true)

    case d: Resolution =>
      standoff += d
      log.info("{}", standoff)
      if(AckState(standoff)){
        log.info("Ackking message")
        context.parent ! Ack(key)
      }
      if(NackState(standoff)) {
        myHttp.client.close()
        log.info("nackked - poisioning")
        self ! PoisonPill
      }
      if(StandoffResolved(standoff)) {
        val time = timeDelta(Some(created), DateTime.now())

        log.info("standoff resolved! Took: {}", time)
        val fi = new File("/tmp/", filename)
        log.info("Deleting {}", fi.toString)
        fi.delete()
        self ! PoisonPill
      }
    case msg =>
      log.info("WorkActor has received a message it cannot match against:{}", sender(), msg)
  }
}
