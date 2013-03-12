package spockdataaccess.ejb.requestsupport.entitycollections;

import java.util.Collection;
import javax.persistence.EntityManager;
import spockdataaccess.ejb.requestsupport.generalisations.BasicEntityCollection;
import spockdataaccess.entity.Network;
import spockdataaccess.entity.NetworkNode;

/**
 *
 * @author Loren Chorley
 */
public class NetworkNodeCollection extends BasicEntityCollection<Network, NetworkNode, Long> {

    public NetworkNodeCollection(EntityManager em) {
        super(em);
    }

    @Override
    protected Collection<NetworkNode> getCollection(Network container) {
        return container.getNetworkNodes();
    }

    @Override
    protected NetworkNode newCollectionEntity(Long id) {
        NetworkNode x = new NetworkNode();
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
        return NetworkNode.class;
    }

    @Override
    protected void verifyBusinessLogic(NetworkNode entity) {
        
    }

}
