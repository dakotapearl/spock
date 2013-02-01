
import javax.ejb.Stateless;

/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

/**
 *
 * @author loren
 */
@Stateless
public class RemoterBean {

    public String getRemoteMessage() {
        return "Here's the remote message";
    }
    
}
