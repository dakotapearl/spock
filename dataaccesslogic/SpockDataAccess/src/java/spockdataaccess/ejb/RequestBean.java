package spockdataaccess.ejb;

import java.util.logging.Level;
import java.util.logging.Logger;
import javax.annotation.PostConstruct;
import javax.ejb.Remove;
import javax.ejb.Stateful;
import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;
import spockdataaccess.ejb.requestsupport.*;

/**
 * Responsible for managing high and low level requests to and from the database.
 * 
 * @author Loren Chorley
 */
@Stateful
public class RequestBean implements RequestBeanLocal, RequestBeanRemote {
    private static final Logger logger = Logger.getLogger("spockdataaccess.ejb.RequestBean");
    
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
    
    private boolean userVerified() {
        if (loggedInUser == null) { 
            return false; 
        } else {
            return true;
        }
    }
    
    @Override
    public ConfigurationFunctions getConfigurationFns() {
        if (!userVerified()) { return null; }
        
        return configurationFns;
    }
    
    @Override
    public NetworkFunctions getNetworkFns() {
        if (!userVerified()) { return null; }
        
        return networkFns;
    }
    
    @Override
    public ExperimentFunctions getExperimentFns() {
        if (!userVerified()) { return null; }
        
        return experimentFns;
    }
    
    @Override
    public EnvironmentFunctions getEnvironmentFns() {
        if (!userVerified()) { return null; }
        
        return environmentFns;
    }
    
    @Override
    public UserFunctions getUserFns() {
        if (!userVerified()) { return null; }
        
        return userFns;
    }
    
    @Override
    public UserInterfaceFunctions getUserInterfaceFns() {
        if (!userVerified()) { return null; }
        
        return userInterfaceFns;
    }
    
    @Override
    public ConnectionFunctions getConnectionFns() {
        if (!userVerified()) { return null; }
        
        return connectionFns;
    }
    
    @Override
    public BehaviourFunctions getBehaviourFns() {
        if (!userVerified()) { return null; }
        
        return behaviourFns;
    }
    
    @Override
    public MetricFunctions getMetricFns() {
        if (!userVerified()) { return null; }
        
        return metricFns;
    }
    
    @PostConstruct
    public void RequestBeanConstruction() {
        configurationFns = new ConfigurationFunctions(em);
        networkFns = new NetworkFunctions(em);
        experimentFns = new ExperimentFunctions(em);
        environmentFns = new EnvironmentFunctions(em);
        userFns = new UserFunctions(em);
        userInterfaceFns = new UserInterfaceFunctions(em);
        connectionFns = new ConnectionFunctions(em);
        behaviourFns = new BehaviourFunctions(em);
        metricFns = new MetricFunctions(em);
        
    }
    
}
