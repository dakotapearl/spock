package spockdataaccess.ejb.requestsupport.entitycollections;

import java.util.Collection;
import javax.persistence.EntityManager;
import spockdataaccess.ejb.requestsupport.generalisations.BasicEntityCollection;
import spockdataaccess.entity.Experiment;
import spockdataaccess.entity.Environment;

/**
 *
 * @author Loren Chorley
 */
public class ExperimentEnvironmentCollection extends BasicEntityCollection<Experiment, Environment, String> {

    public ExperimentEnvironmentCollection(EntityManager em) {
        super(em);
    }

    @Override
    protected Collection<Environment> getCollection(Experiment container) {
        return container.getEnvironments();
    }

    @Override
    protected Environment newCollectionEntity(String id) {
        Environment x = new Environment();
        x.setId(id);
        return x;
    }

    @Override
    protected void copyCollectionEntityProperties(Environment sourceEntity, Environment targetEntity) {
        targetEntity.setIsActive(sourceEntity.getIsActive());
        targetEntity.getExperiments().clear();
        targetEntity.getExperiments().addAll(sourceEntity.getExperiments());
        targetEntity.getEnvironmentNodes().clear();
        targetEntity.getEnvironmentNodes().addAll(sourceEntity.getEnvironmentNodes());
        targetEntity.getEnvironmentInterfaces().clear();
        targetEntity.getEnvironmentInterfaces().addAll(sourceEntity.getEnvironmentInterfaces());
        targetEntity.getMetrics().clear();
        targetEntity.getMetrics().addAll(sourceEntity.getMetrics());
    }

    @Override
    protected String getCollectionEntityID(Environment entity) {
        return entity.getId();
    }

    @Override
    protected Class getCollectionEntityClass() {
        return Environment.class;
    }

    @Override
    protected void verifyBusinessLogic(Environment entity) {
        
    }

}
