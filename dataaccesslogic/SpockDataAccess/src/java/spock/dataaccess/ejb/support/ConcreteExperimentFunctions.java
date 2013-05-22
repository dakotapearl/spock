package spock.dataaccess.ejb.support;

import java.util.logging.Level;
import java.util.logging.Logger;
import javax.ejb.EJB;
import javax.ejb.EJBException;
import javax.ejb.Stateful;
import javax.persistence.EntityManager;
import spock.dataaccess.ejb.interfaces.BasicEntityCollection;
import spock.dataaccess.ejb.interfaces.ExperimentFunctions;
import spock.dataaccess.ejb.interfaces.entities.Environment;
import spock.dataaccess.ejb.interfaces.entities.Experiment;
import spock.dataaccess.ejb.interfaces.entities.Network;
import spock.dataaccess.ejb.interfaces.entities.UserInterface;
import spock.dataaccess.ejb.support.collections.ExperimentEnvironmentCollection;
import spock.dataaccess.ejb.support.collections.ExperimentNetworkCollection;
import spock.dataaccess.ejb.support.collections.ExperimentUserInterfaceCollection;
import spock.dataaccess.entities.ExperimentEntity;

/**
 *
 * @author Loren Chorley
 */
@Stateful
public class ConcreteExperimentFunctions extends AbstractBasicEntity<Experiment, String> implements ExperimentFunctions {
    private static final Logger logger = Logger.getLogger("spockdataaccess.ejb.requestsupport.ExperimentFunctions");
    
    private ExperimentNetworkCollection experimentNetworkCollection;
    private ExperimentEnvironmentCollection experimentEnvironmentCollection;
    private ExperimentUserInterfaceCollection experimentUserInterfaceCollection;
    
    public ConcreteExperimentFunctions() {
        experimentNetworkCollection = new ExperimentNetworkCollection();
        experimentEnvironmentCollection = new ExperimentEnvironmentCollection();
        experimentUserInterfaceCollection = new ExperimentUserInterfaceCollection();
    }
    
    public BasicEntityCollection<Experiment, Network, String> Networks() {
        return experimentNetworkCollection;
    }
    
    public BasicEntityCollection<Experiment, Environment, String> Environments() {
        return experimentEnvironmentCollection;
    }
    
    public BasicEntityCollection<Experiment, UserInterface, Long> UserInterfaces() {
        return experimentUserInterfaceCollection;
    }
    
    @Override
    protected Experiment newEntity(String id) {
        Experiment x = new ExperimentEntity();
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
        return "ExperimentEntity";
    }

    @Override
    protected Class getEntityClass() {
        return ExperimentEntity.class;
    }

    @Override
    protected void verifyBusinessLogic(Experiment entity) {
        
    }
    
}
