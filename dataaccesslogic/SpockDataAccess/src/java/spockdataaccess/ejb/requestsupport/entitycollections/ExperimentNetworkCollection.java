package spockdataaccess.ejb.requestsupport.entitycollections;

import java.util.Collection;
import javax.persistence.EntityManager;
import spockdataaccess.ejb.requestsupport.generalisations.BasicEntityCollection;
import spockdataaccess.entity.Experiment;
import spockdataaccess.entity.Network;

/**
 *
 * @author Loren Chorley
 */
public class ExperimentNetworkCollection extends BasicEntityCollection<Experiment, Network, String> {

    public ExperimentNetworkCollection(EntityManager em) {
        super(em);
    }

    @Override
    protected Collection<Network> getCollection(Experiment container) {
        return container.getNetworks();
    }

    @Override
    protected Network newCollectionEntity(String id) {
        Network x = new Network();
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
        return Network.class;
    }

    @Override
    protected void verifyBusinessLogic(Network entity) {
        
    }

}
