package spock.dataaccess.ejb;

import java.util.logging.Level;
import java.util.logging.Logger;
import javax.annotation.PostConstruct;
import javax.ejb.EJBException;
import javax.ejb.Stateful;
import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;
import spock.dataaccess.ejb.interfaces.*;
import spock.dataaccess.ejb.support.*;

/**
 * Responsible for managing high and low level requests to and from the database.
 * 
 * @author Loren Chorley
 */
@Stateful
public class DataRequestBean implements DataRequest {
    private static final Logger logger = Logger.getLogger("spockdataaccess.ejb.DataRequestBean");
    
    @PersistenceContext
    private EntityManager em;
    
    private ConfigurationFunctions configurationFns;
    private NetworkFunctions networkFns;
    private ExperimentFunctions experimentFns;
    private EnvironmentFunctions environmentFns;
    private UserFunctions userFns;
    private UserInterfaceFunctions userInterfaceFns;
    private ConnectionFunctions connectionFns;
    private BehaviourFunctions behaviourFns;
    private MetricFunctions metricFns;
    
    private String loggedInUser = null;
    private String accessRights = "";
    
    @Override
    public boolean login(String username, String passwordHash) {
        
        logger.log(Level.INFO,
                   "Attempting to log in user {0} with password hash {1}",
                   new Object[] { username, passwordHash });
        
        if (userFns.verifiyUser(username, passwordHash)) {
            
            logger.log(Level.INFO,
                   "User {0} successfully logged in",
                   new Object[] { username });
            
            loggedInUser = username;
            
            // TODO get access rights of user
            
            return true;
        } else {
            
            logger.log(Level.INFO,
                   "User {0} failed to log in",
                   new Object[] { username });
            
            return false;
        }
    }
    
    @Override
    public void sendPasswordResetEmail(String username) {
        
    }
    
    @Override
    public boolean userVerified() {
        if (loggedInUser == null) { 
            return false; 
        } else {
            return true;
        }
    }
    
    @Override
    public ConfigurationFunctions Configuration() {
        if (!userVerified()) { return null; }
        
        return configurationFns;
    }
    
    @Override
    public NetworkFunctions Network() {
        if (!userVerified()) { return null; }
        
        return networkFns;
    }
    
    @Override
    public ExperimentFunctions Experiment() {
        try {    
            if (!userVerified()) { return null; }

            return experimentFns;
        } catch (Exception ex) {
            throw new EJBException("DataRequestBean.Experiment() threw: " + ex.getMessage());
        }
    }
    
    @Override
    public EnvironmentFunctions Environment() {
        if (!userVerified()) { return null; }
        
        return environmentFns;
    }
    
    @Override
    public UserFunctions User() {
        if (!userVerified()) { return null; }
        
        return userFns;
    }
    
    @Override
    public UserInterfaceFunctions UserInterface() {
        if (!userVerified()) { return null; }
        
        return userInterfaceFns;
    }
    
    @Override
    public ConnectionFunctions Connection() {
        if (!userVerified()) { return null; }
        
        return connectionFns;
    }
    
    @Override
    public BehaviourFunctions Behaviour() {
        if (!userVerified()) { return null; }
        
        return behaviourFns;
    }
    
    @Override
    public MetricFunctions Metric() {
        if (!userVerified()) { return null; }
        
        return metricFns;
    }
    
    @PostConstruct
    public void RequestBeanConstruction() {
        
        configurationFns = (ConfigurationFunctions) new ConcreteConfigurationFunctions(em);
        networkFns = (NetworkFunctions) new ConcreteNetworkFunctions(em);
        experimentFns = (ExperimentFunctions) new ConcreteExperimentFunctions(em);
        environmentFns = (EnvironmentFunctions) new ConcreteEnvironmentFunctions(em);
        userFns = (UserFunctions) new ConcreteUserFunctions(em);
        userInterfaceFns = (UserInterfaceFunctions) new ConcreteUserInterfaceFunctions(em);
        connectionFns = (ConnectionFunctions) new ConcreteConnectionFunctions(em);
        behaviourFns = (BehaviourFunctions) new ConcreteBehaviourFunctions(em);
        metricFns = (MetricFunctions) new ConcreteMetricFunctions(em);
        
    }
    
    @Override
    public String returnTestString() {
        return "Spock Data Access test string.";
    }

    @Override
    public NewObjectInterface getNewObject() {
        return new NewObject();
    }
    
}
