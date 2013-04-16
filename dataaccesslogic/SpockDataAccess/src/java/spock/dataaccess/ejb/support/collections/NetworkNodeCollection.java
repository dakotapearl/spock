package spock.dataaccess.ejb.support.collections;

import java.util.Collection;
import javax.persistence.EntityManager;
import spock.dataaccess.ejb.interfaces.entities.Network;
import spock.dataaccess.ejb.interfaces.entities.NetworkNode;
import spock.dataaccess.ejb.support.AbstractBasicEntityCollection;
import spock.dataaccess.entities.NetworkNodeEntity;

/**
 *
 * @author Loren Chorley
 */
public class NetworkNodeCollection extends AbstractBasicEntityCollection<Network, NetworkNode, Long> {

    public NetworkNodeCollection(EntityManager em) {
        super(em);
    }

    @Override
    protected Collection<NetworkNode> getCollection(Network container) {
        return container.getNetworkNodes();
    }

    @Override
    protected NetworkNode newCollectionEntity(Long id) {
        NetworkNode x = new NetworkNodeEntity();
        //x.setId(id);
        return x;
    }

    @Override
    protected void copyCollectionEntityProperties(NetworkNode sourceEntity, NetworkNode targetEntity) {
        targetEntity.setNetwork(sourceEntity.getNetwork());
        targetEntity.setNetworkBehaviour(sourceEntity.getNetworkBehaviour());
        targetEntity.setNetworkInterface(sourceEntity.getNetworkInterface());
    }

    @Override
    protected Long getCollectionEntityID(NetworkNode entity) {
        return entity.getId();
    }

    @Override
    protected Class getCollectionEntityClass() {
        return NetworkNodeEntity.class;
    }

    @Override
    protected void verifyBusinessLogic(NetworkNode entity) {
        
    }

}
