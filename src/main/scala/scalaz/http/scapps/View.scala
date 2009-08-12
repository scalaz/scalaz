package scalaz.http.scapps

import xml.{NodeSeq, Elem, Node}

object ViewHelpers {
  import scalaz.http.request.Request
//
//  def logoutLink(request: Request[Stream]) : Node =
//    (AppEngineAuth.user |> (x => (<a href={AppEngineAuth.logoutUrl(request.path.list.mkString)}>Logout</a>))) | <span> </span>
//
//  def loginLink(request: Request[Stream]) : Node =
//    <a href={AppEngineAuth.loginUrl(request.path.list.mkString)}>Login</a>
//
//  def rowFor(formItem: Elem, label: String, required: Boolean) = {
//    <tr> <td> <label for={formItem.attribute("name")}>{label}</label> </td> <td>{formItem}{required ?? <i>required</i>}</td> </tr>
//  }
//
//  def formForUpdate(action: String, contents: NodeSeq) = {
//    formForCreate(action, <input type="hidden" name="_method" value="put"/> ++ contents)
//  }
//
//  def formForCreate(action: String, contents: NodeSeq) = {
//    <form action={action} method="POST">{contents}</form>
//  }

  object Html {
    def a(url: String, text: String): NodeSeq = <a href={url}>{text}</a>

    def div(x: String): NodeSeq = <div>{x}</div>
  }
}

object View {
  import ViewHelpers._
  import scala.xml.Text
  import scalaz.http.ContentType
  import scalaz.http.response.OK
  import scalaz.http.request.Request
  import scalaz.http.StreamStreamApplication._
  //import _root_.scapps.{JSON, Id}, JSON._, Id._

  implicit val charSet = scalaz.CharSet.UTF8

//  def jsonResponse[T](t: T)(implicit request: Request[Stream], jsoner: JSON[T]) = OK(ContentType, "application/json") << t.jsonString << "\n"
  //def jsonResponse[T](t: T)(implicit request: Request[Stream], jsoner: JSON[T]) = OK(ContentType, "text/plain") << t.jsonString << "\n"

  def doc[A](title: String, content: A, request : Request[Stream]) =
    <html xmlns="http://www.w3.org/1999/xhtml">
    <head>
      <title>{title}</title>
      <meta http-equiv="Content-Type" content="text/html; charset=UTF-8"/>
      <meta name="viewport" content="width = 320px"/>
      <link rel="stylesheet" href="/style.css" media="screen"/>
    </head>
    <body>
      <p>Hello, World.</p>
    </body>
    </html>

}

