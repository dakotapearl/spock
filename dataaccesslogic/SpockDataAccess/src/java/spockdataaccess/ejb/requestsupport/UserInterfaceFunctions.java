package spockdataaccess.ejb.requestsupport;

import spockdataaccess.ejb.requestsupport.generalisations.BasicEntity;
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
public class UserInterfaceFunctions extends BasicEntity<UserInterface, Long> {
    private static final Logger logger = Logger.getLogger("spockdataaccess.ejb.requestsupport.UserInterfaceFunctions");
    
    public UserInterfaceFunctions(EntityManager em) {
        super(em);
    }
    
    @Override
    protected UserInterface newEntity(Long id) {
        try {
            UserInterface x = new UserInterface();
            //x.setId(id);
            return x;
        } catch (Exception ex) {
            throw new EJBException("copyEntityProperties threw: " + ex.getMessage());
        }
    }

    @Override
    protected void copyEntityProperties(UserInterface sourceEntity, UserInterface targetEntity) {
        try {
            targetEntity.setType(sourceEntity.getType());
            targetEntity.setIPAddress(sourceEntity.getIPAddress());
            targetEntity.setUser(sourceEntity.getUser());
            targetEntity.setExperiment(sourceEntity.getExperiment());
        } catch (Exception ex) {
            throw new EJBException("copyEntityProperties threw: " + ex.getMessage());
        }
    }

    @Override
    protected Long getEntityID(UserInterface entity) {
        try {
            return entity.getId();
        } catch (Exception ex) {
            throw new EJBException("copyEntityProperties threw: " + ex.getMessage());
        }
    }

    @Override
    protected String getEntityName() {
        try {
            return "UserInterface";
        } catch (Exception ex) {
            throw new EJBException("copyEntityProperties threw: " + ex.getMessage());
        }
    }

    @Override
    protected Class getEntityClass() {
        try {
            return UserInterface.class;
        } catch (Exception ex) {
            throw new EJBException("copyEntityProperties threw: " + ex.getMessage());
        }
    }
    
    @Override
    protected void verifyBusinessLogic(UserInterface entity) {
        
    }
    
}
