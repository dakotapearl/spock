package spockdataaccess.ejb.requestsupport;

import spockdataaccess.ejb.requestsupport.generalisations.BasicEntity;
import java.util.logging.Logger;
import javax.persistence.EntityManager;
import spockdataaccess.ejb.requestsupport.entitycollections.EnvironmentExperimentCollection;
import spockdataaccess.ejb.requestsupport.entitycollections.EnvironmentInterfaceCollection;
import spockdataaccess.ejb.requestsupport.entitycollections.EnvironmentMetricCollection;
import spockdataaccess.ejb.requestsupport.entitycollections.EnvironmentNodeCollection;
import spockdataaccess.entity.Environment;

/**
 *
 * @author Loren Chorley
 */
public class EnvironmentFunctions extends BasicEntity<Environment, String> {
    private static final Logger logger = Logger.getLogger("spockdataaccess.ejb.requestsupport.EnvironmentFunctions");
    
    public static final boolean INPUT_INTERFACE = true;
    public static final boolean OUTPUT_INTERFACE = false;

    private EnvironmentExperimentCollection environmentExperimentCollection;
    private EnvironmentNodeCollection environmentNodeCollection;
    private EnvironmentInterfaceCollection environmentInterfaceCollection;
    private EnvironmentMetricCollection environmentMetricCollection;
    
    public EnvironmentFunctions(EntityManager em) {
        super(em);
        this.em = em;
        environmentExperimentCollection = new EnvironmentExperimentCollection(em);
        environmentNodeCollection = new EnvironmentNodeCollection(em);
        environmentInterfaceCollection = new EnvironmentInterfaceCollection(em);
        environmentMetricCollection = new EnvironmentMetricCollection(em);
    }
    
    public EnvironmentExperimentCollection Experiments() {
        return environmentExperimentCollection;
    }
    
    public EnvironmentNodeCollection Nodes() {
        return environmentNodeCollection;
    }
    
    public EnvironmentInterfaceCollection Interfaces() {
        return environmentInterfaceCollection;
    }
    
    public EnvironmentMetricCollection Metrics() {
        return environmentMetricCollection;
    }
    
    @Override
    protected Environment newEntity(String id) {
        Environment x = new Environment();
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
        return "Environment";
    }

    @Override
    protected Class getEntityClass() {
        return Environment.class;
    }

    @Override
    protected void verifyBusinessLogic(Environment entity) {
        
    }
    
}
