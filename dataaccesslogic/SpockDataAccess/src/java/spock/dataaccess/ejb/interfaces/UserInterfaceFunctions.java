package spock.dataaccess.ejb.interfaces;

import java.io.Serializable;
import javax.ejb.Remote;
import spock.dataaccess.ejb.interfaces.entities.UserInterface;

/**
 *
 * @author Loren Chorley
 */
@Remote
public interface UserInterfaceFunctions extends Serializable, BasicEntity<UserInterface, Long> {
    
}
