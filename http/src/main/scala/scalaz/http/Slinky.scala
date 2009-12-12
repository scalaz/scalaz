package scalaz
package http

import request.{RequestHeaders, Uris, Methods, Lines}
import response.xhtml.Doctypes
import response.{Bodys, ResponseHeaders}
import servlet.{HttpServlets, HttpServletRequests, HttpServletResponses, HttpSessions}

object Slinky extends EntityHeaders
        with GeneralHeaders
        with Versions
        with Lines
        with Methods
        with RequestHeaders
        with ResponseHeaders
        with Uris
        with StreamStreamApplications
        with Bodys
        with HttpSessions
        with HttpServletResponses
        with HttpServletRequests
        with HttpServlets
        with Doctypes
