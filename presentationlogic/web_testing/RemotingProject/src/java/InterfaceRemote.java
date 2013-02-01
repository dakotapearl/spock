
import javax.ejb.Remote;

/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

/**
 *
 * @author loren
 */
@Remote
public interface InterfaceRemote {
    public String getRemoteMessage();
}
