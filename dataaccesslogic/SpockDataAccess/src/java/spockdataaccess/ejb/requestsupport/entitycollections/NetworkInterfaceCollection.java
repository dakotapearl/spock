package spockdataaccess.ejb.requestsupport.entitycollections;

import java.util.Collection;
import javax.ejb.EJBException;
import javax.persistence.EntityManager;
import spockdataaccess.ejb.requestsupport.generalisations.BasicEntityCollection;
import spockdataaccess.entity.Network;
import spockdataaccess.entity.NetworkInterface;

/**
 *
 * @author Loren Chorley
 */
public class NetworkInterfaceCollection extends BasicEntityCollection<Network, NetworkInterface, Long> {

    public NetworkInterfaceCollection(EntityManager em) {
        super(em);
    }

    @Override
    protected Collection<NetworkInterface> getCollection(Network container) {
        return container.getNetworkInterfaces();
    }

    @Override
    protected NetworkInterface newCollectionEntity(Long id) {
        NetworkInterface x = new NetworkInterface();
        //x.setId(id);
        return x;
    }

    @Override
    protected void copyCollectionEntityProperties(NetworkInterface sourceEntity, NetworkInterface targetEntity) {
        try {
            targetEntity.setNumberOfNodes(sourceEntity.getNumberOfNodes());
            targetEntity.getNetworkNodes().clear();
            targetEntity.getNetworkNodes().addAll(sourceEntity.getNetworkNodes());
            targetEntity.setIsInputInterface(sourceEntity.getIsInputInterface());
            targetEntity.setNetwork(sourceEntity.getNetwork());
        } catch (Exception ex) {
            throw new EJBException("copyCollectionEntityProperties threw: " + ex.getMessage());
        }
    }

    @Override
    protected Long getCollectionEntityID(NetworkInterface entity) {
        return entity.getId();
    }

    @Override
    protected Class getCollectionEntityClass() {
        return NetworkInterface.class;
    }

    @Override
    protected void verifyBusinessLogic(NetworkInterface entity) {
        
    }

}
