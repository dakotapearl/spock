package spock.dataaccess.ejb.interfaces;

import java.io.Serializable;
import spock.dataaccess.ejb.interfaces.entities.NetworkBehaviour;

/**
 * Allows access to the NetworkBehaviour entity and it's database
 * @author Loren Chorley
 */
public interface BehaviourFunctions extends Serializable, BasicEntity<NetworkBehaviour, String> {
    
}
