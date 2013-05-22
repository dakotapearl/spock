package spock.dataaccess.ejb;

import spock.dataaccess.ejb.interfaces.DataRequest;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.annotation.PostConstruct;
import javax.ejb.EJB;
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
    private static final Logger logger = Logger.getLogger("spock.dataaccess.ejb.DataRequestBean");
    
    @PersistenceContext
    public EntityManager em;
    
    @EJB
    private ConfigurationFunctions configurationFns;
    @EJB
    private NetworkFunctions networkFns;
    @EJB
    private ExperimentFunctions experimentFns;
    @EJB
    private EnvironmentFunctions environmentFns;
    @EJB
    private UserFunctions userFns;
    @EJB
    private UserInterfaceFunctions userInterfaceFns;
    @EJB
    private ConnectionFunctions connectionFns;
    @EJB
    private BehaviourFunctions behaviourFns;
    @EJB
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
    public UserFunctions User() {
        if (!userVerified()) { return null; }
        
        return userFns;
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
    public EnvironmentFunctions Environment() {
        if (!userVerified()) { return null; }
        
        return environmentFns;
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
        
        //userFns = (UserFunctions) new ConcreteUserFunctions();
        /*configurationFns = (ConfigurationFunctions) new ConcreteConfigurationFunctions();
        networkFns = (NetworkFunctions) new ConcreteNetworkFunctions();
        experimentFns = (ExperimentFunctions) new ConcreteExperimentFunctions();
        environmentFns = (EnvironmentFunctions) new ConcreteEnvironmentFunctions();
        userInterfaceFns = (UserInterfaceFunctions) new ConcreteUserInterfaceFunctions();
        connectionFns = (ConnectionFunctions) new ConcreteConnectionFunctions();
        behaviourFns = (BehaviourFunctions) new ConcreteBehaviourFunctions();
        metricFns = (MetricFunctions) new ConcreteMetricFunctions();*/
        
        experimentFns.setEntityManager(em);
        userFns.setEntityManager(em);
        configurationFns.setEntityManager(em);
        networkFns.setEntityManager(em);
        environmentFns.setEntityManager(em);
        userInterfaceFns.setEntityManager(em);
        connectionFns.setEntityManager(em);
        behaviourFns.setEntityManager(em);
        metricFns.setEntityManager(em);
        
    }
    
    @Override
    public String returnTestString() {
        return "Spock Data Access test string.";
    }
    
}
