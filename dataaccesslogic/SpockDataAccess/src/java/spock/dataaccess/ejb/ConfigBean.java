package spock.dataaccess.ejb;

import java.io.UnsupportedEncodingException;
import java.math.BigInteger;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.UUID;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.annotation.PostConstruct;
import javax.ejb.EJB;
import javax.ejb.Singleton;
import javax.ejb.Startup;
import spock.dataaccess.ejb.interfaces.EnvironmentFunctions;
import spock.dataaccess.ejb.interfaces.NetworkFunctions;
import spock.dataaccess.ejb.interfaces.UserFunctions;
import spock.dataaccess.ejb.interfaces.entities.*;
import spock.dataaccess.entities.*;

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
    
    static private final boolean staticTestingMode = false;
    static private final boolean dynamicTestingMode = false;
    
    @EJB
    private DataRequest DAR;
    
    @PostConstruct
    public void runTests() {
        
        // Testing login
        if (!DAR.login("root", md5sum("admin"))) {
            logger.log(Level.INFO,
                       "Testing login failed.",
                       new Object[] { });
        } else {
            logger.log(Level.INFO,
                       "Testing login passed.",
                       new Object[] { });
        }
        
        // do testing method if in testing mode
        if (staticTestingMode) {
            if (DAR.login("root", md5sum("admin"))) {
                createStaticData();
            } else {
                logger.log(Level.INFO,
                           "Could not log in.",
                           new Object[] { });
            }
        }
        
        // do dynamic testing method if in testing mode
        if (dynamicTestingMode) {
            if (DAR.login("root", md5sum("admin"))) {
                createDynamicData();
            } else {
                logger.log(Level.INFO,
                           "Could not log in.",
                           new Object[] { });
            }
        }
        
    }
    
    public void createDynamicData() {
        
        logger.log(Level.INFO,
                           "Beginning dynamic testing mode",
                           new Object[] { });
        
        
        Long usercount = DAR.User().countEntities();
        
        logger.log(Level.INFO,
                           "inital user count: {0}",
                           new Object[] { usercount });
        
        
        User newUser = new UserEntity();
        newUser.setUsername("New User " + UUID.randomUUID());
        newUser.setPassword(md5sum("newPassword"));
        newUser.setEmail("new@user.com");
        newUser.setAccessRights(UserFunctions.ACCESSRIGHTS_USER);
        
        DAR.User().setEntity(newUser);
        
        User modifiedUser = (User) DAR.User().retrieveEntity(newUser.getUsername()).get(0);
        
        modifiedUser.setEmail("not_quite_so_new@user.com");
        
        DAR.User().setEntity(modifiedUser);
        
        newUser = new UserEntity();
        
        newUser.setUsername("Another New User");
        newUser.setPassword(md5sum("anotherPassword"));
        newUser.setEmail("another@user.com");
        newUser.setAccessRights(UserFunctions.ACCESSRIGHTS_ADMIN);
        
        DAR.User().setEntity(newUser);
        
        DAR.User().removeEntityByID(newUser.getUsername());
        
        
        usercount = DAR.User().countEntities();
        
        logger.log(Level.INFO,
                           "final user count: {0}",
                           new Object[] { usercount });
        
        Experiment experiment = DAR.Experiment().setEntity(new ExperimentEntity("Experiment " + UUID.randomUUID(), Boolean.FALSE));
        Network network = DAR.Network().setEntity(new NetworkEntity("Network " + UUID.randomUUID(), false));
        Environment environment = DAR.Environment().setEntity(new EnvironmentEntity("Environment " + UUID.randomUUID(), false, "file:///code/e1.jar", "file:///data/e1.dat"));
        
        //RB.Experiment().Networks().setEntityWithinCollection(experiment, network);
        //RB.Experiment().Environments().setEntityWithinCollection(experiment, environment);
        //RB.Network().Experiments().setEntityWithinCollection(network, experiment);
        
        /*experiment.addNetwork(network);
        network.addExperiment(experiment);
        RB.Experiment().setEntity(experiment);
        RB.Network().setEntity(network);*/
        
        // TODO fix
        DAR.Experiment().Networks().setEntityWithinCollection(experiment, network);
        DAR.Experiment().Environments().setEntityWithinCollection(experiment, environment);
        
        NetworkBehaviour behaviour = (NetworkBehaviour) DAR.Behaviour().setEntity(new NetworkBehaviourEntity("Behaviour " + UUID.randomUUID(), "ftp://192.168.0.11/code/behaviour.jar"));
        
        EnvironmentInterface environmentInterface = DAR.Environment().Interfaces().setEntityWithinCollection(environment, new EnvironmentInterfaceEntity(1, environment, EnvironmentFunctions.INPUT_INTERFACE));
        EnvironmentNode enode = DAR.Environment().Nodes().setEntityWithinCollection(environment, new EnvironmentNodeEntity(environment, environmentInterface));
        DAR.Environment().Nodes().setEntityWithinCollection(environment, enode);
        
        NetworkInterface networkInterface = DAR.Network().Interfaces().setEntityWithinCollection(network, new NetworkInterfaceEntity(3, network, NetworkFunctions.OUTPUT_INTERFACE));
        NetworkNode node1 = DAR.Network().Nodes().setEntityWithinCollection(network, new NetworkNodeEntity(network, behaviour, networkInterface));
        NetworkNode node2 = DAR.Network().Nodes().setEntityWithinCollection(network, new NetworkNodeEntity(network, behaviour, networkInterface));
        NetworkNode node3 = DAR.Network().Nodes().setEntityWithinCollection(network, new NetworkNodeEntity(network, behaviour, null));
        DAR.Network().Nodes().setEntityWithinCollection(network, node1);
        DAR.Network().Nodes().setEntityWithinCollection(network, node2);
        DAR.Network().Nodes().setEntityWithinCollection(network, node3);
        
        node3.setNetworkInterface(networkInterface);
        DAR.Network().Nodes().setEntityWithinCollection(network, node3);
        
        DAR.Connection().setEntity(new InterfaceConnectionEntity(networkInterface, environmentInterface, experiment));
        
        experiment.setIsActive(Boolean.TRUE);
        DAR.Experiment().setEntity(experiment);
        
        logger.log(Level.INFO,
                           "Ending dynamic testing mode",
                           new Object[] { });
        
    }
    
    public void createStaticData() {
        logger.log(Level.INFO,
                   "Starting to create testing data",
                   new Object[] { });
        
        logger.log(Level.INFO,
                   "Inserting data without cross-table relationships",
                   new Object[] { });
        
        // Configurations
        DAR.Configuration().setEntity(new ConfigurationEntity("Setting1", "value1"));
        DAR.Configuration().setEntity(new ConfigurationEntity("Setting1", "NewValue"));
        DAR.Configuration().setEntity(new ConfigurationEntity("Setting2", "value2"));
        
        // Experiments
        Experiment experiment1 = DAR.Experiment().setEntity(new ExperimentEntity("Experiment1", Boolean.FALSE));
        Experiment experiment2 = DAR.Experiment().setEntity(new ExperimentEntity("Experiment2", Boolean.FALSE));
        experiment2.setIsActive(true);
        DAR.Experiment().setEntity(experiment2);
        
        // Networks
        Network network1 = DAR.Network().setEntity(new NetworkEntity("Network1", false));
        Network network2 = DAR.Network().setEntity(new NetworkEntity("Network2", false));
        
        // Environments
        Environment environment1 = DAR.Environment().setEntity(new EnvironmentEntity("Environment1", false, "file:///code/e1.jar", "file:///data/e1.dat"));
        Environment environment2 = DAR.Environment().setEntity(new EnvironmentEntity("Environment2", false, "file:///code/e2.jar", "file:///data/e2.dat"));
        
        // Users
        User user1 = DAR.User().setEntity(new UserEntity("Loren", md5sum("MyPassword"), "dakotapearl@gmail.com", "user"));
        
        // User Interfaces
        UserInterface AndroidUI = DAR.UserInterface().setEntity(new UserInterfaceEntity("Android", "192.168.0.11", user1, experiment1));
        DAR.UserInterface().setEntity(new UserInterfaceEntity("Web", "192.168.0.14", null, null));
        // Network Behaviours
        NetworkBehaviour behaviour1 = (NetworkBehaviour) DAR.Behaviour().setEntity(new NetworkBehaviourEntity("RandomBehaviour", "ftp://192.168.0.11/code/behaviour.jar"));
        // Metrics
        Metric metric1 = DAR.Metric().setEntity(new MetricEntity("Cheese-based Metric", "~/metrics/code/cheese.jar"));
        
        logger.log(Level.INFO,
                   "Inserting data that must depend on other data",
                   new Object[] { });
        
        // Environment Interfaces
        EnvironmentInterface environmentInterface = DAR.Environment().Interfaces().setEntityWithinCollection(environment1, new EnvironmentInterfaceEntity(2, environment1, EnvironmentFunctions.INPUT_INTERFACE));
        
        // Network Interfaces
        NetworkInterface networkInterface = DAR.Network().Interfaces().setEntityWithinCollection(network2, new NetworkInterfaceEntity(1, network2, NetworkFunctions.OUTPUT_INTERFACE));
        
        // Network Nodes
        NetworkNode node1 = DAR.Network().Nodes().setEntityWithinCollection(network2, new NetworkNodeEntity(network2, behaviour1, null));
        NetworkNode node2 = DAR.Network().Nodes().setEntityWithinCollection(network2, new NetworkNodeEntity(network2, behaviour1, networkInterface));
        
        
        logger.log(Level.INFO,
                   "Creating relationships between table data",
                   new Object[] { });
        
        // NetworkNode-NetworkNode
        DAR.Network().connectNodes(node1, node2, 2.0);
        
        // NetworkInterface-EnvironmentInterface-Experiment (Interface Connections)
        InterfaceConnection ic = new InterfaceConnectionEntity(networkInterface, environmentInterface, experiment1);
        DAR.Connection().setEntity(ic);
        
        // Experiment-Network
        DAR.Experiment().Networks().setEntityWithinCollection(experiment1, network1);
        
        // Experiment-Environment
        DAR.Experiment().Environments().setEntityWithinCollection(experiment1, environment1);
        
        // Experiment-UserInterface
        AndroidUI.setExperiment(experiment1);
        DAR.UserInterface().setEntity(AndroidUI);
        
        // Check to see if these operations cause the other to happen automatically
        DAR.Experiment().UserInterfaces().setEntityWithinCollection(experiment1, AndroidUI);
        
        // User-UserInterface
        // Check to see if this operation causes the other to happen automatically
        DAR.User().UserInterfaces().setEntityWithinCollection(user1, AndroidUI);
        
        // NetworkNode-NetworkBehaviour
        node2.setNetworkBehaviour(behaviour1);
        DAR.Network().Nodes().setEntityWithinCollection(network2, node2);
        
        // Network-Metric
        DAR.Network().Metrics().setEntityWithinCollection(network2, metric1);
        
        // Environment-Metric
        DAR.Environment().Metrics().setEntityWithinCollection(environment2, metric1);
        
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
