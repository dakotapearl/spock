/*
 * Copyright 2012 Oracle and/or its affiliates.
 * All rights reserved.  You may not modify, use,
 * reproduce, or distribute this software except in
 * compliance with  the terms of the License at:
 * http://developers.sun.com/license/berkeley_license.html
 */


package environment.ejb;

import java.io.UnsupportedEncodingException;
import java.math.BigInteger;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.annotation.PostConstruct;
import javax.ejb.EJB;
import javax.ejb.Singleton;
import javax.ejb.Startup;
import javax.ejb.embeddable.EJBContainer;
import javax.inject.Inject;
import javax.naming.NamingException;
import spockdataaccess.ejb.RequestBean;
import javax.naming.Context;
import javax.naming.InitialContext;
import spockdataaccess.ejb.RequestBeanLocal;
import spockdataaccess.ejb.RequestBeanRemote;

@Singleton
@Startup
public class EnvironmentBean {
    private static final Logger logger = Logger.getLogger("EnvironmentBean");
    
    @EJB
    private RequestBeanRemote requestBeanRemote;
    
    @EJB
    private RequestBean requestBean;
    
    @Inject
    private RequestBeanRemote requestBeanRemotei;
    
    @Inject
    private RequestBean requestBeani;
    
    //private Context ctx;
    //private EJBContainer ec;
    
    @PostConstruct
    public void DoIt() throws NamingException {
        
        //ec = EJBContainer.createEJBContainer();
        //ctx = ec.getContext();
    
        
        logger.log(Level.INFO,
                   "Starting Environment Bean",
                   new Object[] {  });
        
        //Context ctx = new InitialContext();
        
        //RequestBean requestbean = (RequestBean) context.lookup("java:global/SpockDataAccess/SpockDataRequest");
        //RequestBean spockDataRequest = (RequestBean) ctx.lookup("java:global/classes/SpockDataRequest");
        //RequestBean spockDataRequest = (RequestBean) ctx.lookup("java:global/SpockDataAccess/RequestBean");
        //java:module/enterprise bean name
        boolean result = requestBeani.login("root", md5sum("admin"));
        
        logger.log(Level.INFO,
                   "Log in result: {0}",
                   new Object[] { result ? "success" : "failure" });
        
        /*if (ec != null) {
            ec.close();
        }*/
                   
    }
    
    /**
     * Encrypts the given string via md5
     * @param str string to be converted
     * @return returns a hex string
     */
    public String md5sum(String str) {
        String result = "";
        
        try {
            MessageDigest md = MessageDigest.getInstance("MD5");
            byte[] bytesOfMessage = str.getBytes("UTF-8");
            byte[] encryptedPassword = md.digest(bytesOfMessage);
            
            result = new BigInteger(1, encryptedPassword).toString(16);
            
        } catch (UnsupportedEncodingException ex) {
        } catch (NoSuchAlgorithmException ex) {
        } finally {
            return result;
        }
        
    }
    
}
