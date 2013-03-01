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
        
        logger.log(Level.INFO,
                   "Inserting data without cross-table relationships",
                   new Object[] { });
        
        // Experiments
        requestbean.getExperimentFns().createExperiment("Experiment1");
        requestbean.getExperimentFns().createExperiment("Experiment2");
        requestbean.getExperimentFns().setExperimentActivation("Experiment2", true);
        
        // Configurations
        requestbean.getConfigurationFns().setConfiguration("Setting1", "value1");
        requestbean.getConfigurationFns().setConfiguration("Setting1", "NewValue");
        requestbean.getConfigurationFns().setConfiguration("Setting2", "value2");
        
        // Networks
        requestbean.getNetworkFns().createNetwork("Network1");
        requestbean.getNetworkFns().createNetwork("Network2");
        
        // Environments
        requestbean.getEnvironmentFns().createEnvironment("Environment1", "file:///code/e1.jar", "file:///data/e1.dat");
        requestbean.getEnvironmentFns().createEnvironment("Environment2", "file:///code/e2.jar", "file:///data/e2.dat");
        
        // Users
        requestbean.getUserFns().createUser("Loren", "MyPassword", "dakotapearl@gmail.com");
        
        // User Interfaces
        Long AndroidUI_ID = requestbean.getUserInterfaceFns().createUserInterface("Android", "192.168.0.11");
        requestbean.getUserInterfaceFns().createUserInterface("Web", "192.168.0.14");
        
        // Network Behaviours
        // Metrics
        
        logger.log(Level.INFO,
                   "Inserting data that must depend on other data",
                   new Object[] { });
        
        // Network Nodes
        Long node1 = requestbean.getNetworkFns().addNodeToNetwork("Network1");
        Long node2 = requestbean.getNetworkFns().addNodeToNetwork("Network1");
        
        // Environment Interfaces
        requestbean.getEnvironmentFns().createEnvironmentInterface("Environment1", Boolean.TRUE, 3);
        
        // Network Interfaces
        
        logger.log(Level.INFO,
                   "Creating relationships between table data",
                   new Object[] { });
        
        // NetworkNode-NetworkNode
        requestbean.getNetworkFns().connectNodes(node1, node2);
        
        // NetworkNode-NetworkInterface
        
        // NetworkInterface-EnvironmentInterface-Experiment (Connections)
        
        // Experiment-Network
        requestbean.getExperimentFns().addNetworkToExperiment("Experiment1", "Network1");
        
        // Experiment-Environment
        requestbean.getExperimentFns().addEnvironmentToExperiment("Experiment1", "Environment1");
        
        // Experiment-UserInterface
        requestbean.getUserInterfaceFns().setExperimentForUserInterface(AndroidUI_ID, "Experiment2");
        
        // User-UserInterface
        // 
        
        logger.log(Level.INFO,
                   "Finished creating testing data",
                   new Object[] { });
    }
    
    @PreDestroy
    public void deleteData() {
        logger.log(Level.INFO,
                   "Cleaning database. (Warning: Should only be used for testing!)",
                   new Object[] { });
        
        //requestbean.CleanDatabase();
        
        /*requestbean.getUserFns().removeUser("Loren");
        
        requestbean.getExperimentFns().removeEnvironmentFromExperiment("Experiment1", "Environment1");
        requestbean.getExperimentFns().removeNetworkFromExperiment("Experiment1", "Network1");
        
        requestbean.getNetworkFns().removeNetwork("Network1");
        requestbean.getNetworkFns().removeNetwork("Network2");
        
        requestbean.getConfigurationFns().removeConfiguration("setting1");
        requestbean.getConfigurationFns().removeConfiguration("setting2");
        
        requestbean.getExperimentFns().removeExperiment("Experiment1");
        requestbean.getExperimentFns().removeExperiment("Experiment2");*/
        
    }
    
}
