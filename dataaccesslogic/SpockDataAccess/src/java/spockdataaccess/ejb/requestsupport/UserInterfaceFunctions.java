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
public class UserInterfaceFunctions extends BasicEntityFunctions<UserInterface, Long> {
    private static final Logger logger = Logger.getLogger("spockdataaccess.ejb.requestsupport.UserInterfaceFunctions");
    
    private EntityManager em;
    
    public UserInterfaceFunctions(EntityManager em) {
        super(em);
        this.em = em;
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
    
    @Override
    protected UserInterface newEntity(Long id) {
        UserInterface x = new UserInterface();
        x.setId(id);
        return x;
    }

    @Override
    protected void copyEntityProperties(UserInterface sourceEntity, UserInterface targetEntity) {
        targetEntity.setType(sourceEntity.getType());
        targetEntity.setIPAddress(sourceEntity.getIPAddress());
        targetEntity.setUser(sourceEntity.getUser());
        targetEntity.setExperiment(sourceEntity.getExperiment());
    }

    @Override
    protected Long getEntityID(UserInterface entity) {
        return entity.getId();
    }

    @Override
    protected String getEntityName() {
        return "UserInterface";
    }

    @Override
    protected Class getEntityClass() {
        return UserInterface.class;
    }
    
}
