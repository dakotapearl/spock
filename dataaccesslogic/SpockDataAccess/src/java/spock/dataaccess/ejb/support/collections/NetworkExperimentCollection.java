package spock.dataaccess.ejb.support.collections;

import java.util.Collection;
import javax.persistence.EntityManager;
import spock.dataaccess.ejb.interfaces.entities.Experiment;
import spock.dataaccess.ejb.interfaces.entities.Network;
import spock.dataaccess.ejb.support.AbstractBasicEntityCollection;
import spock.dataaccess.entities.ExperimentEntity;
import javax.ejb.Stateful;

/**
 *
 * @author Loren Chorley
 */
//@Stateful
public class NetworkExperimentCollection extends AbstractBasicEntityCollection<Network, Experiment, String> {

    public NetworkExperimentCollection() {
    }

    @Override
    protected Collection<Experiment> getCollection(Network container) {
        return container.getExperiments();
    }

    @Override
    protected Experiment newCollectionEntity(String id) {
        Experiment x = new ExperimentEntity();
        x.setId(id);
        return x;
    }

    @Override
    protected void copyCollectionEntityProperties(Experiment sourceEntity, Experiment targetEntity) {
        targetEntity.setIsActive(sourceEntity.getIsActive());
        targetEntity.getNetworks().clear();
        targetEntity.getNetworks().addAll(sourceEntity.getNetworks());
        targetEntity.getEnvironments().clear();
        targetEntity.getEnvironments().addAll(sourceEntity.getEnvironments());
        targetEntity.getInterfaces().clear();
        targetEntity.getInterfaces().addAll(sourceEntity.getInterfaces());
    }

    @Override
    protected String getCollectionEntityID(Experiment entity) {
        return entity.getId();
    }

    @Override
    protected Class getCollectionEntityClass() {
        return ExperimentEntity.class;
    }

    @Override
    protected void verifyBusinessLogic(Experiment entity) {
        
    }

}
