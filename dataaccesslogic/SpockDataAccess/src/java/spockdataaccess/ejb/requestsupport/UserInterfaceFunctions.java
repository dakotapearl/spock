package spockdataaccess.ejb.requestsupport;

import java.util.logging.Level;
import java.util.logging.Logger;
import javax.ejb.EJBException;
import javax.persistence.EntityManager;
import spockdataaccess.entity.Experiment;
import spockdataaccess.entity.UserInterface;

/**
 *
 * @author Loren Chorley
 */
public class UserInterfaceFunctions {
    private static final Logger logger = Logger.getLogger("spockdataaccess.ejb.requestsupport.UserInterfaceFunctions");
    
    private EntityManager em;
    
    public UserInterfaceFunctions(EntityManager em) {
        this.em = em;
    }
    
    /**
     * Creates a new user interface record in the database.
     * @param Type the type of user interface
     * @param IPAddress the IP address of the user interface
     */
    public long createUserInterface(String Type, String IPAddress) {
        
        try {
            
            UserInterface userInterface = new UserInterface();
            userInterface.setType(Type);
            userInterface.setIPAddress(IPAddress);

            em.persist(userInterface);
            
            logger.log(Level.INFO,
                       "Created and persisted user interface: {0}",
                       new Object[] { userInterface.getId() });
            
            return userInterface.getId();
            
        } catch (Exception ex) {
            throw new EJBException("UserInterfaceFunctions.createUserInterface threw: " + ex.getMessage());
        }
        
    }
    
    /**
     * 
     * @param id
     * @param Type 
     */
    public void setType(Long id, String Type) {
        try {
        
            
        
        } catch (Exception ex) {
            throw new EJBException("UserInterfaceFunctions.setType threw: " + ex.getMessage());
        }
        
    }
    
    /**
     * 
     * @param id
     * @param IPAddress 
     */
    public void setIPAddress(Long id, String IPAddress) {
        try {
        
            
        
        } catch (Exception ex) {
            throw new EJBException("UserInterfaceFunctions.setIPAddress threw: " + ex.getMessage());
        }
        
    }
    
    /**
     * Removes the user interface record from the database.
     * @param id the ID of the user interface
     */
    public void removeUserInterface(Long id) {
        
        try {
        
            UserInterface userInterface = em.find(UserInterface.class, id);
            em.remove(userInterface);
            
            logger.log(Level.INFO,
                       "Removed user interface: {0}",
                       new Object[] { id });
            
        } catch (Exception ex) {
            throw new EJBException("UserInterfaceFunctions.removeUserInterface threw: " + ex.getMessage());
        }
        
    }
    
    /**
     * 
     * @param id
     * @param experimentId 
     */
    public void setExperimentForUserInterface(Long id, String experimentId) {
        try {
        
            UserInterface userInterface = em.find(UserInterface.class, id);
            Experiment experiment = em.find(Experiment.class, experimentId);
            
            if (experiment == null) {
                throw new EJBException("setExperiment threw: Experiment '" + experimentId + "' does not exist.");
            } else if (userInterface == null) {
                throw new EJBException("setExperiment threw: UserInterface '" + id + "' does not exist.");
            }
            
            userInterface.setExperiment(experiment);
        
            logger.log(Level.INFO,
                       "set experiment {0} to UI {1}",
                       new Object[] { experiment.getId(), userInterface.getId() });
            
        } catch (Exception ex) {
            throw new EJBException("UserInterfaceFunctions.setIPAddress threw: " + ex.getMessage());
        }
        
    }
    
}
