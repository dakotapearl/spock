package spockdataaccess.ejb;

import java.util.logging.Level;
import java.util.logging.Logger;
import javax.annotation.PostConstruct;
import javax.annotation.PreDestroy;
import javax.ejb.Stateful;
import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;
import spockdataaccess.ejb.requestsupport.*;

/**
 *
 * Responsible for managing high and low level requests to and from the database.
 * 
 * @author Loren Chorley
 */
@Stateful
public class RequestBean {
    private static final Logger logger = Logger.getLogger("spockdataaccess.ejb.RequestBean");
    
    @PersistenceContext
    private EntityManager em;
    
    private ConfigurationFunctions configurationFns;
    private NetworkFunctions networkFns;
    private ExperimentFunctions experimentFns;
    private EnvironmentFunctions environmentFns;
    private UserFunctions userFns;
    private UserInterfaceFunctions userInterfaceFns;
    
    public ConfigurationFunctions getConfigurationFns() {
        return configurationFns;
    }
    
    public NetworkFunctions getNetworkFns() {
        return networkFns;
    }
    
    public ExperimentFunctions getExperimentFns() {
        return experimentFns;
    }
    
    public EnvironmentFunctions getEnvironmentFns() {
        return environmentFns;
    }
    
    public UserFunctions getUserFns() {
        return userFns;
    }
    
    public UserInterfaceFunctions getUserInterfaceFns() {
        return userInterfaceFns;
    }
    
    @PostConstruct
    public void RequestBeanConstruction() {
        configurationFns = new ConfigurationFunctions(em);
        networkFns = new NetworkFunctions(em);
        experimentFns = new ExperimentFunctions(em);
        environmentFns = new EnvironmentFunctions(em);
        userFns = new UserFunctions(em);
        userInterfaceFns = new UserInterfaceFunctions(em);
        
    }
    
    @PreDestroy
    public void CleanDatabase() {
        try {
            
            em.createQuery("DROP TABLE Experiments_Environments").executeUpdate();
            em.createQuery("DROP TABLE Experiments_Networks").executeUpdate();
            em.createQuery("DROP TABLE NetworkNodes_NetworkNodes").executeUpdate();
            em.flush();
            
            em.createQuery("DROP TABLE Configurations").executeUpdate();
            em.createQuery("DROP TABLE NetworkNodes").executeUpdate();
            em.createQuery("DROP TABLE Networks").executeUpdate();
            em.createQuery("DROP TABLE Environments").executeUpdate();
            em.createQuery("DROP TABLE Experiments").executeUpdate();
            em.createQuery("DROP TABLE UserInterfaces").executeUpdate();
            
            em.createQuery("DROP TABLE Users").executeUpdate();
            
            em.createQuery("DROP TABLE UserInterfaces").executeUpdate();
            em.createQuery("DROP TABLE Experiments").executeUpdate();
            em.createQuery("DROP TABLE Environments").executeUpdate();
            em.createQuery("DROP TABLE Networks").executeUpdate();
            em.createQuery("DROP TABLE NetworkNodes").executeUpdate();
            em.createQuery("DROP TABLE Configurations").executeUpdate();
        } catch (Exception ex) {
            
        }
    }
    
    
}
