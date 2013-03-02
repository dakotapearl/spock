package spockdataaccess.ejb.requestsupport;

import java.util.logging.Level;
import java.util.logging.Logger;
import javax.ejb.EJBException;
import javax.persistence.EntityManager;
import spockdataaccess.entity.Configuration;
import spockdataaccess.entity.EnvironmentInterface;
import spockdataaccess.entity.Experiment;
import spockdataaccess.entity.InterfaceConnection;
import spockdataaccess.entity.NetworkInterface;

/**
 * Reponsible for managing connections between environment and network interfaces
 * @author Loren Chorley
 */
public class ConnectionFunctions {
    private static final Logger logger = Logger.getLogger("spockdataaccess.ejb.requestsupport.ConnectionFunctions");
    
    public static final int NETWORK_INTERFACE = 0;
    public static final int ENVIRONMENT_INTERFACE = 1;
    
    private EntityManager em;
    
    public ConnectionFunctions(EntityManager em) {
        this.em = em;
    }
    
    /**
     * Create connection between two interfaces. Note currently does not support env->env or net->net connections
     * @param outputInterfaceId
     * @param outputInterfaceType
     * @param inputInterfaceId
     * @param inputInterfaceType
     * @param experimentId 
     */
    public void createConnection(
            Long outputInterfaceId, 
            int outputInterfaceType, 
            Long inputInterfaceId, 
            int inputInterfaceType,
            String experimentId) {
        
        try {
            
            // Add verification: 
            //   types are 0 or 1
            //   interfaces are input and output
            //   experiment exists
            //   no current connection exactly the same
            //   whether the interface are compatible (the same number of nodes)
            
            
            Experiment experiment = em.find(Experiment.class, experimentId);
            
            NetworkInterface networkInterface = null;
            EnvironmentInterface environmentInterface = null;
            
            if (outputInterfaceType == NETWORK_INTERFACE) {
                networkInterface = em.find(NetworkInterface.class, outputInterfaceId);
            } else if (outputInterfaceType == ENVIRONMENT_INTERFACE) {
                environmentInterface = em.find(EnvironmentInterface.class, outputInterfaceId);
            } else {
                //throw something
            }
            
            if (inputInterfaceId == NETWORK_INTERFACE) {
                networkInterface = em.find(NetworkInterface.class, inputInterfaceId);
            } else if (inputInterfaceId == ENVIRONMENT_INTERFACE) {
                environmentInterface = em.find(EnvironmentInterface.class, inputInterfaceId);
            } else {
                //throw something
            }
            
            InterfaceConnection interfaceConnection = new InterfaceConnection();
            interfaceConnection.setNetworkInterface(networkInterface);
            interfaceConnection.setEnvironmentInterface(environmentInterface);
            interfaceConnection.setExperiment(experiment);
            
            em.persist(interfaceConnection);
            
            logger.log(Level.INFO,
                       "Created connection between interfaces: {0}->{1} for experiment {2}",
                       new Object[] { outputInterfaceId, inputInterfaceId, experimentId });
            
        } catch (Exception ex) {
            throw new EJBException("ConnectionFunctions.createConnection threw: " + ex.getMessage());
        }
        
    }
    
    /**
     * 
     * @param name
     * @return 
     */
    public String getConnection(String name) {
        String rtnval = null;
        
        try {
            
            
            
        } catch (Exception ex) {
            throw new EJBException("ConnectionFunctions.getConnection threw: " + ex.getMessage());
        } finally {
            return rtnval;
        }
        
    }
    
    /**
     * 
     * @param name 
     */
    public void removeConnection(String name) {
        try {
            
            
            
            logger.log(Level.INFO,
                       "Removed configuration: {0}",
                       new Object[] { name });
            
        } catch (Exception ex) {
            throw new EJBException("ConnectionFunctions.removeConnection threw: " + ex.getMessage());
        }
    }
    
}
