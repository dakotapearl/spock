package spockdataaccess.ejb.requestsupport;

import java.util.logging.Logger;
import javax.persistence.EntityManager;
import spockdataaccess.ejb.requestsupport.generalisations.BasicEntity;
import spockdataaccess.entity.NetworkBehaviour;

/**
 * Allows access to the NetworkBehaviour entity and it's database
 * @author Loren Chorley
 */
public class BehaviourFunctions extends BasicEntity<NetworkBehaviour, String> {
    private static final Logger logger = Logger.getLogger("spockdataaccess.ejb.requestsupport.BehaviourFunctions");
    
    public BehaviourFunctions(EntityManager em) {
        super(em);
    }
    
    @Override
    protected NetworkBehaviour newEntity(String id) {
        NetworkBehaviour x = new NetworkBehaviour();
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
        return "NetworkBehaviour";
    }

    @Override
    protected Class getEntityClass() {
        return NetworkBehaviour.class;
    }

    @Override
    protected void verifyBusinessLogic(NetworkBehaviour entity) {
        
    }
    
}
