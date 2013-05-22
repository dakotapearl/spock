package spock.dataaccess.ejb.interfaces;

import java.io.Serializable;
import javax.ejb.Remote;
import spock.dataaccess.ejb.interfaces.entities.NetworkBehaviour;

/**
 * Allows access to the NetworkBehaviour entity and it's database
 * @author Loren Chorley
 */
@Remote
public interface BehaviourFunctions extends Serializable, BasicEntity<NetworkBehaviour, String> {
    
}
