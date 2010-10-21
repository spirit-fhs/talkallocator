/**
 * Copyright (c) 2010 Oliver Braun
 *
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * 3. Neither the name of the author nor the names of his contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHORS ``AS IS'' AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
 * STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
 * ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */
package org.obraun.talkallocator
package model

import net.liftweb.mapper._
import net.liftweb.common._
import net.liftweb.http.S
import net.liftweb.util.Helpers._
import net.liftweb.util._

class User extends MegaProtoUser[User] {
  def getSingleton = User
  object fhsid extends MappedString(this,100)
  object title extends MappedString(this,100)
}

object User extends User with MetaMegaProtoUser[User] {
  override def dbTableName = "users"

  override def menus = sitemap
  override lazy val sitemap = List(loginMenuLoc, logoutMenuLoc).flatten(a => a)

  override def loginXhtml = {
    (<lift:surround with="default" at ="content">
      <form method="post" action={S.uri}>
        <table>
          <tr><td colspan="2">{S.??("log.in")}</td></tr>
          <tr><td>{S.??("FHS-ID")}</td><td><user:user /></td></tr>
          <tr><td>{S.??("password")}</td><td><user:password /></td></tr>
          <td><user:submit /></td>
        </table>
      </form>
     </lift:surround>)
   }

   override def login = {
     if (S.post_?) {
       if (S.param("username").open_!.equals("") || S.param("password").open_!.equals("")){
        S.error("Errorcode: Bitte User und Pass angeben")
        S.redirectTo("/user_mgt/login")
       }
       if(LDAPAuth.tryLogin(S.param("username").open_!,S.param("password").open_!)){
          println("[LDAP] -----------------> Login Successfull!")
          User.logUserIdIn(S.param("username").open_!)
          if (User.find(By(firstName, S.param("username").open_!)).isEmpty) {
            val thisUser = create
            thisUser.email(S.getSessionAttribute("email").openOr("oops!"))
            thisUser.fhsid(S.param("username"))
            thisUser.title(S.getSessionAttribute("fullname").openOr("shit happens").split(" ")(0))
            thisUser.firstName(S.getSessionAttribute("fullname").openOr("shit happens").split(" ")(1))
            thisUser.lastName(S.getSessionAttribute("fullname").openOr("shit happens").split(" ")(2))
            if(S.param("username").open_! == "denison" || S.param("username").open_! == "braun3") {
              thisUser.superUser(true)
            }
            thisUser.validated(true).save
            println("User created....")
            User.logUserIn(thisUser)
          } else {
            println(S.param("username").open_! + " has logged in before!")
            User.logUserIn(User.find(By(fhsid, S.param("username").open_!)).open_!)
          }
          S.notice("Login Successful as "+ S.getSessionAttribute("fullname").openOr("Go AWAY!"))
          S.redirectTo("/index")
       } else {
        println("[LDAP] -----------------> Login not Successfull!")
       }
     }

     bind("user", loginXhtml,
          "user" -> ((<input type="text" name="username"/>)),
          "password" -> (<input type="password" name="password"/>),
          "submit" -> (<input type="submit" value={S.??("log.in")}/>))
   }
}

