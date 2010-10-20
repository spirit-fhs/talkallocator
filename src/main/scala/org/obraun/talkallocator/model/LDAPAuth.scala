package org.obraun.talkallocator.model

import javax.naming._
import javax.naming.directory._
import java.util._
import net.liftweb.http.S

object LDAPAuth {
  private val authEnv = new Hashtable[String,String]

  System.setProperty("javax.net.ssl.trustStore", "fhstore")

  private def getEmail(ctx: DirContext, dn: String): String = {
    val attrs: Attributes = ctx.getAttributes(dn)
    val enumDn = attrs.getIDs
    var email = ""
    while(enumDn.hasMore){
      val tmp = enumDn.next
      val attr = attrs.get(tmp)
      for(e <- 0 to attr.size-1){
        if(tmp.equals("mail") && attr.get(e).toString.matches("[a-zA-Z][.].\\w.*@fh-sm.de")){
          email = attr.get(e).toString
        }
      }
    }
  email
  }

  private def buildGoodName(ctx: DirContext, dn: String): String = {
    val attrs: Attributes = ctx.getAttributes(dn)
    val enumDn = attrs.getIDs
    var returnName = ""
    var sname = ""
    var pT = "";
    while(enumDn.hasMore){
      val tmp = enumDn.next
      val attr = attrs.get(tmp)
      for(e <- 0 to attr.size-1){
        if(tmp.equals("personalTitle")){
          pT += attr.get(e).toString
        }
        if(tmp.equals("cn")){
          sname += attr.get(e).toString
        }
      }
    }
  pT + " " + sname
  }

  private def emailValidator(email: String, userName:String): String = email match{
    case "" => userName + "@stud.fh-sm.de"
    case _ => email
  }

  def tryLogin(userName: String, passWord: String): Boolean = {
    var base = ""
    if(userName == "braun3"){
      base = "ou=people,dc=fh-sm,dc=de"
    } else {
      base = "ou=students,dc=fh-sm,dc=de"
    }
    val dn = "uid=" + userName + "," + base
    val ldapURL = "ldaps://ldap1.fh-schmalkalden.de:636"

    authEnv.put(Context.INITIAL_CONTEXT_FACTORY, "com.sun.jndi.ldap.LdapCtxFactory")
    authEnv.put(Context.PROVIDER_URL, ldapURL);
   	authEnv.put(Context.SECURITY_AUTHENTICATION, "simple");
   	authEnv.put(Context.SECURITY_PRINCIPAL, dn);
   	authEnv.put(Context.SECURITY_CREDENTIALS, passWord);
    authEnv.put(Context.SECURITY_PROTOCOL, "SSL")

    try{
      val ctx: DirContext = new InitialDirContext(authEnv)
      S.setSessionAttribute("fullname", buildGoodName(ctx, dn))
      S.setSessionAttribute("email", emailValidator(getEmail(ctx, dn), userName))
      true
    }catch{
      case e: AuthenticationException => e
        println("[LDAP] -----------------> Login not Successfull!\n" + e)
        S.error("Errorcode: Bitte richtigen User und Pass angeben")
        S.redirectTo("/user_mgt/login")
        false
      case b: NamingException => b
        println("[LDAP] -----------------> Login not Successfull\n!" + b)
        S.error("Errorcode: I can't see LDAP, please contact a SPIRIT-Admin")
        S.redirectTo("/user_mgt/login")
        false
      case c: TimeLimitExceededException => c
        println("[LDAP] -----------------> " + ldapURL + " not available")
        S.error("Errorcode: I can't see LDAP, please contact a SPIRIT-Admin")
        false
     }
  }
}
