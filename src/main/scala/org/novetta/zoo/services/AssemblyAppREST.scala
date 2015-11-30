package org.novetta.zoo.services

import dispatch.Defaults._
import dispatch.{url, _}
import org.json4s.JsonAST.{JString, JValue}
import org.novetta.zoo.types.{TaskedWork, WorkFailure, WorkResult, WorkSuccess}
import collection.mutable


case class AssemblyAppWork(key: Long, filename: String, TimeoutMillis: Int, WorkType: String, Worker: String, Arguments: List[String]) extends TaskedWork {
  def doWork()(implicit myHttp: dispatch.Http): Future[WorkResult] = {

    val uri = AssemblyAppREST.constructURL(Worker, filename, Arguments)
    val requestResult = myHttp(url(uri) OK as.String)
      .either
      .map({
      case Right(content) =>
        AssemblyAppSuccess(true, JString(content), Arguments)

      case Left(StatusCode(404)) =>
        AssemblyAppFailure(false, JString("Not found (File already deleted?)"), Arguments)

      case Left(StatusCode(500)) =>
        AssemblyAppFailure(false, JString("AssemblyApp service failed, check local logs"), Arguments) //would be ideal to print response body here

      case Left(StatusCode(code)) =>
        AssemblyAppFailure(false, JString("Some other code: " + code.toString), Arguments)

      case Left(something) =>
        AssemblyAppFailure(false, JString("wildcard failure: " + something.toString), Arguments)
    })
    requestResult
  }
}


case class AssemblyAppSuccess(status: Boolean, data: JValue, Arguments: List[String], routingKey: String = "assemblyapp.result.static.totem", WorkType: String = "ASSEMBLYAPP") extends WorkSuccess
case class AssemblyAppFailure(status: Boolean, data: JValue, Arguments: List[String], routingKey: String = "", WorkType: String = "ASSEMBLYAPP") extends WorkFailure


object AssemblyAppREST {
  def constructURL(root: String, filename: String, arguments: List[String]): String = {
    arguments.foldLeft(new mutable.StringBuilder(root+filename))({
      (acc, e) => acc.append(e)}).toString()
  }
}