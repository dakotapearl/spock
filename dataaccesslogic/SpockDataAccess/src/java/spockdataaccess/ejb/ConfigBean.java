package spockdataaccess.ejb;

import java.io.UnsupportedEncodingException;
import java.math.BigInteger;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.annotation.PostConstruct;
import javax.annotation.PreDestroy;
import javax.ejb.EJB;
import javax.ejb.Singleton;
import javax.ejb.Startup;
import spockdataaccess.ejb.requestsupport.ConnectionFunctions;
import spockdataaccess.ejb.requestsupport.EnvironmentFunctions;
import spockdataaccess.ejb.requestsupport.NetworkFunctions;
import spockdataaccess.ejb.requestsupport.UserFunctions;
import spockdataaccess.entity.User;

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
    
    static private final boolean testingMode = true;
    
    @EJB
    private RequestBeanLocal requestbean;
    
    @PostConstruct
    public void checkUsers() {
        
        // do testing method if in testing mode
        if (testingMode) {
            if (requestbean.login("root", md5sum("admin"))) {
                createData();
            } else {
                logger.log(Level.INFO,
                           "Could not log in.",
                           new Object[] { });
            }
        }
        
    }
    
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
        requestbean.getUserFns().createUser("Loren", "MyPassword", "dakotapearl@gmail.com", "user");
        
        // User Interfaces
        Long AndroidUI_ID = requestbean.getUserInterfaceFns().createUserInterface("Android", "192.168.0.11");
        requestbean.getUserInterfaceFns().createUserInterface("Web", "192.168.0.14");
        
        // Network Behaviours
        requestbean.getBehaviourFns().setBehaviour("RandomBehaviour", "ftp://192.168.0.11/code/behaviour.jar");
        
        // Metrics
        requestbean.getMetricFns().setMetric("Cheese-based Metric", "~/metrics/code/cheese.jar");
        
        logger.log(Level.INFO,
                   "Inserting data that must depend on other data",
                   new Object[] { });
        
        // Network Nodes
        Long node1 = requestbean.getNetworkFns().addNodeToNetwork("Network1");
        Long node2 = requestbean.getNetworkFns().addNodeToNetwork("Network1");
        
        // Environment Interfaces
        Long EnvironmentInterfaceID = requestbean.getEnvironmentFns().createEnvironmentInterface("Environment1", EnvironmentFunctions.INPUT_INTERFACE, 3);
        
        // Network Interfaces
        Long NetworkInterfaceID = requestbean.getNetworkFns().createNetworkInterface("Network1", NetworkFunctions.OUTPUT_INTERFACE, 3);
        
        logger.log(Level.INFO,
                   "Creating relationships between table data",
                   new Object[] { });
        
        // NetworkNode-NetworkNode
        requestbean.getNetworkFns().connectNodes(node1, node2, 2.0);
        
        // NetworkInterface-EnvironmentInterface-Experiment (Interface Connections)
        requestbean.getConnectionFns().createConnection(
                NetworkInterfaceID, 
                ConnectionFunctions.NETWORK_INTERFACE, 
                EnvironmentInterfaceID, 
                ConnectionFunctions.ENVIRONMENT_INTERFACE, 
                "Experiment2");
        
        // Experiment-Network
        requestbean.getExperimentFns().addNetworkToExperiment("Experiment1", "Network1");
        
        // Experiment-Environment
        requestbean.getExperimentFns().addEnvironmentToExperiment("Experiment1", "Environment1");
        
        // Experiment-UserInterface
        requestbean.getUserInterfaceFns().setExperimentForUserInterface(AndroidUI_ID, "Experiment2");
        
        // User-UserInterface
        
        // NetworkNode-NetworkBehaviour
        
        // Network-Metric
        
        // Environment-Metric
        
        
        logger.log(Level.INFO,
                   "Finished creating testing data",
                   new Object[] { });
    }
    
    /**
     * Encrypts the given string via md5
     * @param str string to be converted
     * @return returns a hex string
     */
    public String md5sum(String str) {
        String result = "";
        
        try {
            MessageDigest md = MessageDigest.getInstance("MD5");
            byte[] bytesOfMessage = str.getBytes("UTF-8");
            byte[] encryptedPassword = md.digest(bytesOfMessage);
            
            result = new BigInteger(1, encryptedPassword).toString(16);
            
        } catch (UnsupportedEncodingException ex) {
        } catch (NoSuchAlgorithmException ex) {
        } finally {
            return result;
        }
        
    }
    
}
