package spockdataaccess.ejb.requestsupport.entitycollections;

import java.util.Collection;
import javax.persistence.EntityManager;
import spockdataaccess.ejb.requestsupport.generalisations.BasicEntityCollection;
import spockdataaccess.entity.Environment;
import spockdataaccess.entity.Experiment;

/**
 *
 * @author Loren Chorley
 */
public class EnvironmentExperimentCollection extends BasicEntityCollection<Environment, Experiment, String> {

    public EnvironmentExperimentCollection(EntityManager em) {
        super(em);
    }

    @Override
    protected Collection<Experiment> getCollection(Environment container) {
        return container.getExperiments();
    }

    @Override
    protected Experiment newCollectionEntity(String id) {
        Experiment x = new Experiment();
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
        return Experiment.class;
    }

    @Override
    protected void verifyBusinessLogic(Experiment entity) {
        
    }

}
