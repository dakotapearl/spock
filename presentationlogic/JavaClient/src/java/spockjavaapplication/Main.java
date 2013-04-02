/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package spockjavaapplication;

import java.io.UnsupportedEncodingException;
import java.math.BigInteger;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import javax.ejb.EJB;
import spockdataaccess.ejb.RequestBeanRemote;

/**
 *
 * @author loren
 */
public class Main {

    @EJB
    private static RequestBeanRemote request;

    public Main(String[] args) {
    }

    public static void main(String[] args) {
        Main client = new Main(args);

        try {
            request.login("root", md5sum("admin"));

            System.exit(0);
        } catch (Exception ex) {
            System.err.println("Caught an exception:");
        }
    }
    
    /**
     * Encrypts the given string via md5
     * @param str string to be converted
     * @return returns a hex string
     */
    public static String md5sum(String str) {
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
