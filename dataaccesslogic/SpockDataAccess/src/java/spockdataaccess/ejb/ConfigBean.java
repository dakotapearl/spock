package spockdataaccess.ejb;

import java.util.logging.Level;
import java.util.logging.Logger;
import javax.annotation.PostConstruct;
import javax.annotation.PreDestroy;
import javax.ejb.EJB;
import javax.ejb.Singleton;
import javax.ejb.Startup;

/**
 * This bean is solely used for testing purposes and should not be use in a 
 * production environment as it will delete the current database when undeplyed.
 * 
 * @author Loren Chorley
 */
@Singleton
@Startup
public class ConfigBean {
    private static final Logger logger = Logger.getLogger("spockdataaccess.ejb.ConfigBean");
    
    @EJB
    private RequestBean requestbean;
    
    @PostConstruct
    public void createData() {
        logger.log(Level.INFO,
                   "Starting to create testing data",
                   new Object[] { });
        
        requestbean.getExperimentFns().createExperiment("Experiment1");
        requestbean.getExperimentFns().createExperiment("Experiment2");
        requestbean.getExperimentFns().setExperimentActivation("Experiment2", true);
        
        requestbean.getConfigurationFns().setConfiguration("Setting1", "value1");
        
        logger.log(Level.INFO,
                   "Requested value of Setting1: {0}",
                   new Object[] { requestbean.getConfigurationFns().getConfiguration("Setting1") });
        
        requestbean.getConfigurationFns().removeConfiguration("Setting1");
        requestbean.getConfigurationFns().setConfiguration("Setting2", "value2");
        
        logger.log(Level.INFO,
                   "Removed first configuration and added a new one",
                   new Object[] {  });
        
        requestbean.getNetworkFns().createNetwork("Network1");
        requestbean.getNetworkFns().createNetwork("Network2");
        
        requestbean.getEnvironmentFns().createEnvironment("Environment1", "file:///code/e1.jar", "file:///data/e1.dat");
        
        requestbean.getExperimentFns().addNetworkToExperiment("Experiment1", "Network1");
        requestbean.getExperimentFns().addEnvironmentToExperiment("Experiment1", "Environment1");
        
        logger.log(Level.INFO,
                   "Finished creating testing data",
                   new Object[] { });
    }
    
    @PreDestroy
    public void deleteData() {
        logger.log(Level.INFO,
                   "Cleaning database. (Warning: Should only be used for testing!)",
                   new Object[] { });
        
        requestbean.getExperimentFns().removeEnvironmentFromExperiment("Experiment1", "Environment1");
        requestbean.getExperimentFns().removeNetworkFromExperiment("Experiment1", "Network1");
        
        requestbean.getNetworkFns().removeNetwork("Network1");
        requestbean.getNetworkFns().removeNetwork("Network2");
        
        requestbean.getConfigurationFns().removeConfiguration("setting2");
        
        requestbean.getExperimentFns().removeExperiment("Experiment1");
        requestbean.getExperimentFns().removeExperiment("Experiment2");
        
    }
    
}
