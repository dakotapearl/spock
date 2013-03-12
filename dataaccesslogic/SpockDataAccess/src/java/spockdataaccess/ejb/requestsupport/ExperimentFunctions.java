package spockdataaccess.ejb.requestsupport;

import spockdataaccess.ejb.requestsupport.generalisations.BasicEntity;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.ejb.EJBException;
import javax.persistence.EntityManager;
import spockdataaccess.ejb.requestsupport.entitycollections.ExperimentEnvironmentCollection;
import spockdataaccess.ejb.requestsupport.entitycollections.ExperimentNetworkCollection;
import spockdataaccess.ejb.requestsupport.entitycollections.ExperimentUserInterfaceCollection;
import spockdataaccess.entity.Environment;
import spockdataaccess.entity.Experiment;

/**
 *
 * @author Loren Chorley
 */
public class ExperimentFunctions extends BasicEntity<Experiment, String> {
    private static final Logger logger = Logger.getLogger("spockdataaccess.ejb.requestsupport.ExperimentFunctions");
    
    private ExperimentNetworkCollection experimentNetworkCollection;
    private ExperimentEnvironmentCollection experimentEnvironmentCollection;
    private ExperimentUserInterfaceCollection experimentUserInterfaceCollection;
    
    public ExperimentFunctions(EntityManager em) {
        super(em);
        this.em = em;
        experimentNetworkCollection = new ExperimentNetworkCollection(em);
        experimentEnvironmentCollection = new ExperimentEnvironmentCollection(em);
        experimentUserInterfaceCollection = new ExperimentUserInterfaceCollection(em);
    }
    
    public ExperimentNetworkCollection Networks() {
        return experimentNetworkCollection;
    }
    
    public ExperimentEnvironmentCollection Environments() {
        return experimentEnvironmentCollection;
    }
    
    public ExperimentUserInterfaceCollection UserInterfaces() {
        return experimentUserInterfaceCollection;
    }
    
    @Override
    protected Experiment newEntity(String id) {
        Experiment x = new Experiment();
        x.setId(id);
        return x;
    }

    @Override
    protected void copyEntityProperties(Experiment sourceEntity, Experiment targetEntity) {
        if (sourceEntity == null) {
            throw new EJBException("source entity was passed null for experiment functions");
        }
        if (targetEntity == null) {
            throw new EJBException("target entity was passed null for experiment functions");
        }
        
        try {
            targetEntity.setIsActive(sourceEntity.getIsActive());
            targetEntity.getEnvironments().clear();
            targetEntity.getEnvironments().addAll(sourceEntity.getEnvironments());
            targetEntity.getInterfaces().clear();
            targetEntity.getInterfaces().addAll(sourceEntity.getInterfaces());
            targetEntity.getNetworks().clear();
            targetEntity.getNetworks().addAll(sourceEntity.getNetworks());
        } catch (Exception ex) {
            throw new EJBException("copyEntityProperties threw: " + ex.getMessage());
        }
    }

    @Override
    protected String getEntityID(Experiment entity) {
        return entity.getId();
    }

    @Override
    protected String getEntityName() {
        return "Experiment";
    }

    @Override
    protected Class getEntityClass() {
        return Experiment.class;
    }

    @Override
    protected void verifyBusinessLogic(Experiment entity) {
        
    }
    
}
