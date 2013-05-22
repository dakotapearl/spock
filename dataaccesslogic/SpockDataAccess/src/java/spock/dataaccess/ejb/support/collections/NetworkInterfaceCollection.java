package spock.dataaccess.ejb.support.collections;

import java.util.Collection;
import javax.ejb.EJBException;
import javax.persistence.EntityManager;
import spock.dataaccess.ejb.interfaces.entities.Network;
import spock.dataaccess.ejb.interfaces.entities.NetworkInterface;
import spock.dataaccess.ejb.support.AbstractBasicEntityCollection;
import spock.dataaccess.entities.NetworkInterfaceEntity;
import javax.ejb.Stateful;

/**
 *
 * @author Loren Chorley
 */
//@Stateful
public class NetworkInterfaceCollection extends AbstractBasicEntityCollection<Network, NetworkInterface, Long> {

    public NetworkInterfaceCollection() {
    }

    @Override
    protected Collection<NetworkInterface> getCollection(Network container) {
        return container.getNetworkInterfaces();
    }

    @Override
    protected NetworkInterface newCollectionEntity(Long id) {
        NetworkInterface x = new NetworkInterfaceEntity();
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
        return NetworkInterfaceEntity.class;
    }

    @Override
    protected void verifyBusinessLogic(NetworkInterface entity) {
        
    }

}
