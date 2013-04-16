package spock.dataaccess.ejb.support.collections;

import java.util.Collection;
import javax.persistence.EntityManager;
import spock.dataaccess.ejb.interfaces.entities.Experiment;
import spock.dataaccess.ejb.interfaces.entities.Network;
import spock.dataaccess.ejb.support.AbstractBasicEntityCollection;
import spock.dataaccess.entities.NetworkEntity;

/**
 *
 * @author Loren Chorley
 */
public class ExperimentNetworkCollection extends AbstractBasicEntityCollection<Experiment, Network, String> {

    public ExperimentNetworkCollection(EntityManager em) {
        super(em);
    }

    @Override
    protected Collection<Network> getCollection(Experiment container) {
        return container.getNetworks();
    }

    @Override
    protected Network newCollectionEntity(String id) {
        Network x = new NetworkEntity();
        x.setId(id);
        return x;
    }

    @Override
    protected void copyCollectionEntityProperties(Network sourceEntity, Network targetEntity) {
        targetEntity.setIsActive(sourceEntity.getIsActive());
        targetEntity.getExperiments().clear();
        targetEntity.getExperiments().addAll(sourceEntity.getExperiments());
        targetEntity.getNetworkNodes().clear();
        targetEntity.getNetworkNodes().addAll(sourceEntity.getNetworkNodes());
        targetEntity.getNetworkInterfaces().clear();
        targetEntity.getNetworkInterfaces().addAll(sourceEntity.getNetworkInterfaces());
        targetEntity.getMetrics().clear();
        targetEntity.getMetrics().addAll(sourceEntity.getMetrics());
    }

    @Override
    protected String getCollectionEntityID(Network entity) {
        return entity.getId();
    }

    @Override
    protected Class getCollectionEntityClass() {
        return NetworkEntity.class;
    }

    @Override
    protected void verifyBusinessLogic(Network entity) {
        
    }

}
