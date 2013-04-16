package spock.dataaccess.ejb.support.collections;

import java.util.Collection;
import javax.persistence.EntityManager;
import spock.dataaccess.ejb.interfaces.entities.Environment;
import spock.dataaccess.ejb.interfaces.entities.Experiment;
import spock.dataaccess.ejb.support.AbstractBasicEntityCollection;
import spock.dataaccess.entities.EnvironmentEntity;

/**
 *
 * @author Loren Chorley
 */
public class ExperimentEnvironmentCollection extends AbstractBasicEntityCollection<Experiment, Environment, String> {

    public ExperimentEnvironmentCollection(EntityManager em) {
        super(em);
    }

    @Override
    protected Collection<Environment> getCollection(Experiment container) {
        return container.getEnvironments();
    }

    @Override
    protected Environment newCollectionEntity(String id) {
        Environment x = new EnvironmentEntity();
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
        return EnvironmentEntity.class;
    }

    @Override
    protected void verifyBusinessLogic(Environment entity) {
        
    }

}
