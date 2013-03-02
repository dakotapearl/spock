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
import spockdataaccess.ejb.RequestBean;
import spockdataaccess.ejb.RequestBeanLocal;
import spockdataaccess.ejb.RequestBeanRemote;

@Singleton
@Startup
public class EnvironmentBean {
    private static final Logger logger = Logger.getLogger("EnvironmentBean");

    @EJB
    private RequestBeanRemote requestbean;
    
    @PostConstruct
    public void DoIt() {
        boolean result = requestbean.login("root", md5sum("admin"));
        
        logger.log(Level.INFO,
                   "Log in result: {0}",
                   new Object[] { result ? "success" : "failure" });
        
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
