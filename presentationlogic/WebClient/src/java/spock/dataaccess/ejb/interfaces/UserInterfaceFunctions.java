package spock.dataaccess.ejb.interfaces;

import java.io.Serializable;
import spock.dataaccess.ejb.interfaces.entities.UserInterface;

/**
 *
 * @author Loren Chorley
 */
public interface UserInterfaceFunctions extends Serializable, BasicEntity<UserInterface, Long> {
    
}
