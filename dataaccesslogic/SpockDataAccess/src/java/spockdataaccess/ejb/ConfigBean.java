package spockdataaccess.ejb;

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
import spockdataaccess.ejb.requestsupport.*;
import spockdataaccess.entity.*;

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
    static private final boolean dynamicTestingMode = true;
    
    @EJB
    private RequestBeanLocal RB;
    
    @PostConstruct
    public void checkUsers() {
        
        // do testing method if in testing mode
        if (staticTestingMode) {
            if (RB.login("root", md5sum("admin"))) {
                createStaticData();
            } else {
                logger.log(Level.INFO,
                           "Could not log in.",
                           new Object[] { });
            }
        }
        
        // do dynamic testing method if in testing mode
        if (dynamicTestingMode) {
            if (RB.login("root", md5sum("admin"))) {
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
        
        
        Long usercount = RB.User().countEntities();
        
        logger.log(Level.INFO,
                           "inital user count: {0}",
                           new Object[] { usercount });
        
        
        User newUser = new User();
        newUser.setUsername("New User " + UUID.randomUUID());
        newUser.setPassword(md5sum("newPassword"));
        newUser.setEmail("new@user.com");
        newUser.setAccessRights(UserFunctions.ACCESSRIGHTS_USER);
        
        RB.User().setEntity(newUser);
        
        User modifiedUser = (User) RB.User().retrieveEntity(newUser.getUsername()).get(0);
        
        modifiedUser.setEmail("not_quite_so_new@user.com");
        
        RB.User().setEntity(modifiedUser);
        
        newUser = new User();
        
        newUser.setUsername("Another New User");
        newUser.setPassword(md5sum("anotherPassword"));
        newUser.setEmail("another@user.com");
        newUser.setAccessRights(UserFunctions.ACCESSRIGHTS_ADMIN);
        
        RB.User().setEntity(newUser);
        
        RB.User().removeEntity(newUser.getUsername());
        
        
        usercount = RB.User().countEntities();
        
        logger.log(Level.INFO,
                           "final user count: {0}",
                           new Object[] { usercount });
        
        Experiment experiment = RB.Experiment().setEntity(new Experiment("Experiment " + UUID.randomUUID(), Boolean.FALSE));
        Network network = RB.Network().setEntity(new Network("Network " + UUID.randomUUID(), false));
        Environment environment = RB.Environment().setEntity(new Environment("Environment " + UUID.randomUUID(), false, "file:///code/e1.jar", "file:///data/e1.dat"));
        
        //RB.Experiment().Networks().setEntityWithinCollection(experiment, network);
        //RB.Experiment().Environments().setEntityWithinCollection(experiment, environment);
        //RB.Network().Experiments().setEntityWithinCollection(network, experiment);
        
        /*experiment.addNetwork(network);
        network.addExperiment(experiment);
        RB.Experiment().setEntity(experiment);
        RB.Network().setEntity(network);*/
        
        // TODO fix
        RB.Experiment().Networks().setEntityWithinCollection(experiment, network);
        RB.Experiment().Environments().setEntityWithinCollection(experiment, environment);
        
        NetworkBehaviour behaviour = RB.Behaviour().setEntity(new NetworkBehaviour("Behaviour " + UUID.randomUUID(), "ftp://192.168.0.11/code/behaviour.jar"));
        
        EnvironmentInterface environmentInterface = RB.Environment().Interfaces().setEntityWithinCollection(environment, new EnvironmentInterface(1, environment, EnvironmentFunctions.INPUT_INTERFACE));
        EnvironmentNode enode = RB.Environment().Nodes().setEntityWithinCollection(environment, new EnvironmentNode(environment, environmentInterface));
        RB.Environment().Nodes().setEntityWithinCollection(environment, enode);
        
        NetworkInterface networkInterface = RB.Network().Interfaces().setEntityWithinCollection(network, new NetworkInterface(3, network, NetworkFunctions.OUTPUT_INTERFACE));
        NetworkNode node1 = RB.Network().Nodes().setEntityWithinCollection(network, new NetworkNode(network, behaviour, networkInterface));
        NetworkNode node2 = RB.Network().Nodes().setEntityWithinCollection(network, new NetworkNode(network, behaviour, networkInterface));
        NetworkNode node3 = RB.Network().Nodes().setEntityWithinCollection(network, new NetworkNode(network, behaviour, null));
        RB.Network().Nodes().setEntityWithinCollection(network, node1);
        RB.Network().Nodes().setEntityWithinCollection(network, node2);
        RB.Network().Nodes().setEntityWithinCollection(network, node3);
        
        node3.setNetworkInterface(networkInterface);
        RB.Network().Nodes().setEntityWithinCollection(network, node3);
        
        RB.Connection().setEntity(new InterfaceConnection(networkInterface, environmentInterface, experiment));
        
        experiment.setIsActive(Boolean.TRUE);
        RB.Experiment().setEntity(experiment);
        
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
        RB.Configuration().setEntity(new Configuration("Setting1", "value1"));
        RB.Configuration().setEntity(new Configuration("Setting1", "NewValue"));
        RB.Configuration().setEntity(new Configuration("Setting2", "value2"));
        
        // Experiments
        Experiment experiment1 = RB.Experiment().setEntity(new Experiment("Experiment1", Boolean.FALSE));
        Experiment experiment2 = RB.Experiment().setEntity(new Experiment("Experiment2", Boolean.FALSE));
        experiment2.setIsActive(true);
        RB.Experiment().setEntity(experiment2);
        
        // Networks
        Network network1 = RB.Network().setEntity(new Network("Network1", false));
        Network network2 = RB.Network().setEntity(new Network("Network2", false));
        
        // Environments
        Environment environment1 = RB.Environment().setEntity(new Environment("Environment1", false, "file:///code/e1.jar", "file:///data/e1.dat"));
        Environment environment2 = RB.Environment().setEntity(new Environment("Environment2", false, "file:///code/e2.jar", "file:///data/e2.dat"));
        
        // Users
        User user1 = RB.User().setEntity(new User("Loren", md5sum("MyPassword"), "dakotapearl@gmail.com", "user"));
        
        // User Interfaces
        UserInterface AndroidUI = RB.UserInterface().setEntity(new UserInterface("Android", "192.168.0.11", user1, experiment1));
        RB.UserInterface().setEntity(new UserInterface("Web", "192.168.0.14", null, null));
        // Network Behaviours
        NetworkBehaviour behaviour1 = RB.Behaviour().setEntity(new NetworkBehaviour("RandomBehaviour", "ftp://192.168.0.11/code/behaviour.jar"));
        // Metrics
        Metric metric1 = RB.Metric().setEntity(new Metric("Cheese-based Metric", "~/metrics/code/cheese.jar"));
        
        logger.log(Level.INFO,
                   "Inserting data that must depend on other data",
                   new Object[] { });
        
        // Environment Interfaces
        EnvironmentInterface environmentInterface = RB.Environment().Interfaces().setEntityWithinCollection(environment1, new EnvironmentInterface(2, environment1, EnvironmentFunctions.INPUT_INTERFACE));
        
        // Network Interfaces
        NetworkInterface networkInterface = RB.Network().Interfaces().setEntityWithinCollection(network2, new NetworkInterface(1, network2, NetworkFunctions.OUTPUT_INTERFACE));
        
        // Network Nodes
        NetworkNode node1 = RB.Network().Nodes().setEntityWithinCollection(network2, new NetworkNode(network2, behaviour1, null));
        NetworkNode node2 = RB.Network().Nodes().setEntityWithinCollection(network2, new NetworkNode(network2, behaviour1, networkInterface));
        
        
        logger.log(Level.INFO,
                   "Creating relationships between table data",
                   new Object[] { });
        
        // NetworkNode-NetworkNode
        RB.Network().connectNodes(node1, node2, 2.0);
        
        // NetworkInterface-EnvironmentInterface-Experiment (Interface Connections)
        InterfaceConnection ic = new InterfaceConnection(networkInterface, environmentInterface, experiment1);
        RB.Connection().setEntity(ic);
        
        // Experiment-Network
        RB.Experiment().Networks().setEntityWithinCollection(experiment1, network1);
        
        // Experiment-Environment
        RB.Experiment().Environments().setEntityWithinCollection(experiment1, environment1);
        
        // Experiment-UserInterface
        AndroidUI.setExperiment(experiment1);
        RB.UserInterface().setEntity(AndroidUI);
        
        // Check to see if these operations cause the other to happen automatically
        RB.Experiment().UserInterfaces().setEntityWithinCollection(experiment1, AndroidUI);
        
        // User-UserInterface
        // Check to see if this operation causes the other to happen automatically
        RB.User().UserInterfaces().setEntityWithinCollection(user1, AndroidUI);
        
        // NetworkNode-NetworkBehaviour
        node2.setNetworkBehaviour(behaviour1);
        RB.Network().Nodes().setEntityWithinCollection(network2, node2);
        
        // Network-Metric
        RB.Network().Metrics().setEntityWithinCollection(network2, metric1);
        
        // Environment-Metric
        RB.Environment().Metrics().setEntityWithinCollection(environment2, metric1);
        
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
