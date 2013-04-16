package spock.dataaccess.ejb.support;

import java.util.logging.Logger;
import javax.persistence.EntityManager;
import spock.dataaccess.ejb.interfaces.BasicEntityCollection;
import spock.dataaccess.ejb.interfaces.EnvironmentFunctions;
import spock.dataaccess.ejb.interfaces.entities.Environment;
import spock.dataaccess.ejb.interfaces.entities.EnvironmentInterface;
import spock.dataaccess.ejb.interfaces.entities.EnvironmentNode;
import spock.dataaccess.ejb.interfaces.entities.Experiment;
import spock.dataaccess.ejb.interfaces.entities.Metric;
import spock.dataaccess.ejb.support.collections.EnvironmentExperimentCollection;
import spock.dataaccess.ejb.support.collections.EnvironmentInterfaceCollection;
import spock.dataaccess.ejb.support.collections.EnvironmentMetricCollection;
import spock.dataaccess.ejb.support.collections.EnvironmentNodeCollection;
import spock.dataaccess.entities.EnvironmentEntity;

/**
 *
 * @author Loren Chorley
 */
public class ConcreteEnvironmentFunctions extends AbstractBasicEntity<Environment, String> implements EnvironmentFunctions {
    private static final Logger logger = Logger.getLogger("spockdataaccess.ejb.requestsupport.EnvironmentFunctions");
    
    private EnvironmentExperimentCollection environmentExperimentCollection;
    private EnvironmentNodeCollection environmentNodeCollection;
    private EnvironmentInterfaceCollection environmentInterfaceCollection;
    private EnvironmentMetricCollection environmentMetricCollection;
    
    public ConcreteEnvironmentFunctions(EntityManager em) {
        super(em);
        this.em = em;
        environmentExperimentCollection = new EnvironmentExperimentCollection(em);
        environmentNodeCollection = new EnvironmentNodeCollection(em);
        environmentInterfaceCollection = new EnvironmentInterfaceCollection(em);
        environmentMetricCollection = new EnvironmentMetricCollection(em);
    }
    
    public BasicEntityCollection<Environment, Experiment, String> Experiments() {
        return environmentExperimentCollection;
    }
    
    public BasicEntityCollection<Environment, EnvironmentNode, Long> Nodes() {
        return environmentNodeCollection;
    }
    
    public BasicEntityCollection<Environment, EnvironmentInterface, Long> Interfaces() {
        return environmentInterfaceCollection;
    }
    
    public BasicEntityCollection<Environment, Metric, String> Metrics() {
        return environmentMetricCollection;
    }
    
    @Override
    protected Environment newEntity(String id) {
        Environment x = new EnvironmentEntity();
        x.setId(id);
        return x;
    }

    @Override
    protected void copyEntityProperties(Environment sourceEntity, Environment targetEntity) {
        targetEntity.setIsActive(sourceEntity.getIsActive());
        targetEntity.setCodeURL(sourceEntity.getCodeURL());
        targetEntity.setDataURL(sourceEntity.getDataURL());
        targetEntity.getExperiments().clear();
        targetEntity.getExperiments().addAll(sourceEntity.getExperiments());
        targetEntity.getEnvironmentInterfaces().clear();
        targetEntity.getEnvironmentInterfaces().addAll(sourceEntity.getEnvironmentInterfaces());
        targetEntity.getEnvironmentNodes().clear();
        targetEntity.getEnvironmentNodes().addAll(sourceEntity.getEnvironmentNodes());
    }

    @Override
    protected String getEntityID(Environment entity) {
        return entity.getId();
    }

    @Override
    protected String getEntityName() {
        return "EnvironmentEntity";
    }

    @Override
    protected Class getEntityClass() {
        return EnvironmentEntity.class;
    }

    @Override
    protected void verifyBusinessLogic(Environment entity) {
        
    }
    
}
