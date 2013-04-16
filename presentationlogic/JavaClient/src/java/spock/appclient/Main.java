package spock.appclient;

import java.io.UnsupportedEncodingException;
import java.math.BigInteger;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.ejb.EJB;

/**
 *
 * @author Loren Chorley
 */
public class Main {
    private static final Logger logger = Logger.getLogger("spock.appclient.Main");
    
    //@EJB
    //private static TestBean testbean;

    public Main() {
    }

    public static void main(String[] args) {
        Main client = new Main();
        
        System.out.println("Java client started");
        logger.log(Level.INFO,
                   "Java client component started!",
                   new Object[] { });

        //testbean.start();

        client.startSimulation();

    }
    
    public void startSimulation() {
        //System.out.println("testbean.start() = " + testbean.start());
    }
    
    /**
     * Encrypts the given string via md5
     * @param str string to be converted
     * @return returns a hex string
     */
    /*public static String md5sum(String str) {
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
        
    }*/
    
}
