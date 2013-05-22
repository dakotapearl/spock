package spock.webclient.ejb;

import java.io.UnsupportedEncodingException;
import java.math.BigInteger;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.ArrayList;
import java.util.Collection;
import javax.ejb.EJB;
import javax.ejb.EJBException;
import javax.enterprise.context.RequestScoped;
import javax.inject.Named;
import spock.business.ejb.BusinessInterfaceRequest;
import spock.dataaccess.ejb.interfaces.DataRequest;
import spock.dataaccess.ejb.interfaces.SrzedObjInt;
import spock.dataaccess.ejb.interfaces.entities.User;
import spock.environment.ejb.EnvironmentRequest;
import spock.network.ejb.NetworkRequest;

/**
 *
 * @author Loren Chorley
 */
@Named
@RequestScoped
public class config {

    protected String title = "Spock Web Client";
    
    @EJB
    private DataRequest dataRequest;
    
    @EJB
    private NetworkRequest networkRequest;
    
    @EJB
    private EnvironmentRequest environmentRequest;
    
    @EJB
    private BusinessInterfaceRequest businessInterfaceRequest;
    
    /**
     * Get the value of title
     * @return the title of the web project
     */
    public String getTitle() {
        return title;
    }
    
    public Collection<String> getTestStrings() {
        
        Collection<String> rtn = new ArrayList<String>();
        rtn.add(dataRequest.returnTestString());
        rtn.add(environmentRequest.returnTestString());
        rtn.add(networkRequest.returnTestString());
        rtn.add(businessInterfaceRequest.returnTestString());
        return rtn;
    }
    
    public Collection<String> getContent() {
        try {
            String tmp = "";
            Collection<String> rtn = new ArrayList<String>();
            rtn.add("Login " + ((dataRequest.login("root", md5sum("admin"))) ? "successful." : "failed."));
            rtn.add("User verified: " + ((dataRequest.userVerified()) ? "yes" : "no"));
            
            //dataRequest.User().removeEntityByID("root"); //Problem
            
            //tmp = dataRequest.User().retrieveEntity("root").get(0).getAccessRights(); //Problem
            //rtn.add("Root user access rights are: " + tmp);
            
            rtn.add("User count: " + dataRequest.User().countEntities());
            
            //User rootuser = (User) dataRequest.User().getRootUserAccessRights();
            //rtn.add("Root user access right: " + rootuser.getAccessRights());
            
            
            Object obj = dataRequest.User().getRootUserAccessRights();
            
            //rtn.add("Root user access right: " + );
            
            return rtn;
        } catch (Exception ex) {
            throw new EJBException("config.getContent() threw: " + ex.getMessage() + " (" + ex.toString() + ")");
        }
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
