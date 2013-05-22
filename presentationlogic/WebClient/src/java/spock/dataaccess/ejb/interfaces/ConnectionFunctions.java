package spock.dataaccess.ejb.interfaces;

import java.io.Serializable;
import javax.ejb.Remote;
import spock.dataaccess.ejb.interfaces.entities.InterfaceConnection;

/**
 * Reponsible for managing connections between environment and network interfaces
 * @author Loren Chorley
 */
@Remote
public interface ConnectionFunctions extends Serializable, BasicEntity<InterfaceConnection, Long> {
    
}
