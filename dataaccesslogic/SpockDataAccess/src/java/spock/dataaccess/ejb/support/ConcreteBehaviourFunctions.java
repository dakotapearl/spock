package spock.dataaccess.ejb.support;

import java.util.logging.Logger;
import javax.persistence.EntityManager;
import spock.dataaccess.ejb.interfaces.BehaviourFunctions;
import spock.dataaccess.ejb.interfaces.entities.NetworkBehaviour;
import spock.dataaccess.entities.NetworkBehaviourEntity;
import javax.ejb.Stateful;

/**
 * Allows access to the NetworkBehaviour entity and it's database
 * @author Loren Chorley
 */
@Stateful
public class ConcreteBehaviourFunctions extends AbstractBasicEntity<NetworkBehaviour, String> implements BehaviourFunctions {
    private static final Logger logger = Logger.getLogger("spockdataaccess.ejb.requestsupport.BehaviourFunctions");
    
    public ConcreteBehaviourFunctions() {
    }
    
    @Override
    protected NetworkBehaviour newEntity(String id) {
        NetworkBehaviour x = new NetworkBehaviourEntity();
        x.setId(id);
        return x;
    }

    @Override
    protected void copyEntityProperties(NetworkBehaviour sourceEntity, NetworkBehaviour targetEntity) {
        targetEntity.setCodeURL(sourceEntity.getCodeURL());
    }

    @Override
    protected String getEntityID(NetworkBehaviour entity) {
        return entity.getId();
    }

    @Override
    protected String getEntityName() {
        return "NetworkBehaviourEntity";
    }

    @Override
    protected Class getEntityClass() {
        return NetworkBehaviourEntity.class;
    }

    @Override
    protected void verifyBusinessLogic(NetworkBehaviour entity) {
        
    }
    
}
