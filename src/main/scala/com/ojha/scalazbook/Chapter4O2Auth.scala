package com.ojha.scalazbook

import eu.timepit.refined.api.Refined
import eu.timepit.refined.string.Url
import spray.json.{DefaultJsonProtocol, ImplicitDerivedFormats}


object Parser extends App with DefaultJsonProtocol with ImplicitDerivedFormats {


  final case class AuthRequest(
                                redirect_uri: String Refined Url,
                                scope: String,
                                client_id: String,
                                prompt: String = "consent",
                                response_type: String = "code",
                                access_type: String = "offline"
                              )

  final case class AccessRequest(
                                  code: String,
                                  redirect_uri: String Refined Url,
                                  client_id: String,
                                  client_secret: String,
                                  scope: String = "",
                                  grant_type: String = "authorization_code"
                                )

  final case class AccessResponse(
                                   access_token: String,
                                   token_type: String,
                                   expires_in: Long,
                                   refresh_token: String
                                 )

  final case class RefreshRequest(
                                   client_secret: String,
                                   refresh_token: String,
                                   client_id: String,
                                   grant_type: String = "refresh_token"
                                 )

  final case class RefreshResponse(
                                    access_token: String,
                                    token_type: String,
                                    expires_in: Long
                                  )


}

object MainSample extends App with DefaultJsonProtocol with ImplicitDerivedFormats {

  case class A(x: Int)

  case class B(a: A, str: String)

  private val json = B(A(42), "hello world").toJson
  val x: B = json.convertTo[B]
//  val rawr = implicitly(JsonReader[B])
  println(json.prettyPrint)

}
