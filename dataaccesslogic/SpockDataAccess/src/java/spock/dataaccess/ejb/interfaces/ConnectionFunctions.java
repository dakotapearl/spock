package spock.dataaccess.ejb.interfaces;

import java.io.Serializable;
import spock.dataaccess.ejb.interfaces.entities.InterfaceConnection;

/**
 * Reponsible for managing connections between environment and network interfaces
 * @author Loren Chorley
 */
public interface ConnectionFunctions extends Serializable, BasicEntity<InterfaceConnection, Long> {
    
}
