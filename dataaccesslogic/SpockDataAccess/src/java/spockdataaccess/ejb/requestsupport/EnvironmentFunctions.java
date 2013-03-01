package spockdataaccess.ejb.requestsupport;

import java.util.logging.Level;
import java.util.logging.Logger;
import javax.ejb.EJBException;
import javax.persistence.EntityManager;
import spockdataaccess.entity.Environment;
import spockdataaccess.entity.EnvironmentInterface;
import spockdataaccess.entity.EnvironmentNode;

/**
 *
 * @author Loren Chorley
 */
public class EnvironmentFunctions {
    private static final Logger logger = Logger.getLogger("spockdataaccess.ejb.requestsupport.EnvironmentFunctions");
    
    private EntityManager em;
    
    public EnvironmentFunctions(EntityManager em) {
        this.em = em;
    }
    
    /**
     * Creates a new environment record in the database.
     * @param id the ID of the environment
     */
    public void createEnvironment(
            String id,
            String CodeURL,
            String DataURL) {
        
        try {
        
            Environment environment = new Environment();
            environment.setId(id);
            environment.setIsActive(Boolean.FALSE);
            environment.setCodeURL(CodeURL);
            environment.setDataURL(DataURL);

            em.persist(environment);
            
            logger.log(Level.INFO,
                       "Created and persisted environment: {0}",
                       new Object[] { id });
            
        } catch (Exception ex) {
            throw new EJBException("RequestBean.createEnvironment threw: " + ex.getMessage());
        }
        
    }
    
    /**
     * Sets the activation status of an environment. True indicates that the environment is currently active or running and may be stopped, and false that it is available to be started.
     * @param id the ID of the environment
     * @param activation the new activation status of the environment
     */
    public void setEnvironmentActivation(String id, boolean activation) {
        try {
        
            Environment environment = em.find(Environment.class, id);
            
            if (environment != null) {
                environment.setIsActive(activation);
            } else {
                throw new EJBException("Environment '" + id + "' not found when attempting to set activation property");
            }
        
        } catch (Exception ex) {
            throw new EJBException("RequestBean.setEnvironmentActivation threw: " + ex.getMessage());
        }
        
    }
    
    /**
     * Sets the CodeURL of an environment.
     * @param id the ID of the environment
     * @param CodeURL the URL of the binary code that makes up the environment functionality
     */
    public void setEnvironmentCodeURL(String id, String CodeURL) {
        try {
        
            Environment environment = em.find(Environment.class, id);
            
            if (environment != null) {
                environment.setCodeURL(CodeURL);
            } else {
                throw new EJBException("Environment '" + id + "' not found when attempting to set CodeURL property");
            }
        
        } catch (Exception ex) {
            throw new EJBException("RequestBean.setEnvironmentActivation threw: " + ex.getMessage());
        }
        
    }
    
    /**
     * Sets the DataURL of an environment.
     * @param id the ID of the environment
     * @param DataURL the URL of the data that helps to define the environment
     */
    public void setEnvironmentDataURL(String id, String DataURL) {
        try {
        
            Environment environment = em.find(Environment.class, id);
            
            if (environment != null) {
                environment.setDataURL(DataURL);
            } else {
                throw new EJBException("Environment '" + id + "' not found when attempting to set DataURL property");
            }
        
        } catch (Exception ex) {
            throw new EJBException("RequestBean.setEnvironmentActivation threw: " + ex.getMessage());
        }
        
    }
    
    /**
     * Removes the environment record from the database.
     * @param id the ID of the environment
     */
    public void removeEnvironment(String id) {
        
        try {
        
            Environment environment = em.find(Environment.class, id);
            em.remove(environment);
            
            logger.log(Level.INFO,
                       "Removed environment: {0}",
                       new Object[] { id });
            
        } catch (Exception ex) {
            throw new EJBException("RequestBean.removeEnvironment threw: " + ex.getMessage());
        }
        
    }
    
    /**
     * Creates an environment interface, the right amount of environment nodes and connects them all appropriately
     * @param environmentId the environment for the environment interface
     * @param interfaceIsInput If this is true then the interface is for input, otherwise it is for output
     * @param numberOfNodes determines the number of nodes that are attached to this interface
     * @return returns the interface ID
     */
    public Long createEnvironmentInterface(
            String environmentId,
            Boolean interfaceIsInput,
            Integer numberOfNodes) {
        
        try {
            
            Environment environment = em.find(Environment.class, environmentId);
            
            // Create interface
            EnvironmentInterface environmentInterface = new EnvironmentInterface();
            
            environmentInterface.setIsInputInterface(interfaceIsInput);
            environmentInterface.setNumberOfNodes(numberOfNodes);

            // Add interface to environment, and vise versa
            environment.addEnvironmentInterface(environmentInterface);
            environmentInterface.setEnvironment(environment);
            
            em.persist(environmentInterface);
            
            // Create exactly the right number of environment nodes and add them to both the interface and the enivornemnt
            EnvironmentNode environmentNode;
            for (int i = 0; i<numberOfNodes; i++) {
                environmentNode = new EnvironmentNode();
                
                environment.addEnvironmentNode(environmentNode);
                environmentNode.setEnvironment(environment);
                
                environmentInterface.addEnvironmentNode(environmentNode);
                environmentNode.setEnvironmentInterface(environmentInterface);
                
                em.persist(environmentNode);
                
            }
            
            
            logger.log(Level.INFO,
                       "Created and persisted environment interface: {0}",
                       new Object[] { environmentInterface.getId() });
            
            return environmentInterface.getId();
            
        } catch (Exception ex) {
            throw new EJBException("RequestBean.createEnvironment threw: " + ex.getMessage());
        }
        
    }
    
    /**
     * 
     * @param environmentInterfaceId 
     */
    public void removeEnvironmentInterface(Long environmentInterfaceId) {
        try {
            
        } catch (Exception ex) {
            throw new EJBException("RequestBean.createEnvironment threw: " + ex.getMessage());
        }
    }
    
}
