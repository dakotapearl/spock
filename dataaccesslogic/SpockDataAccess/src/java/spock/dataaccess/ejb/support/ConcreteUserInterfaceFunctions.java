package spock.dataaccess.ejb.support;

import java.util.logging.Logger;
import javax.ejb.EJBException;
import javax.ejb.Stateful;
import javax.persistence.EntityManager;
import spock.dataaccess.ejb.interfaces.UserInterfaceFunctions;
import spock.dataaccess.ejb.interfaces.entities.UserInterface;
import spock.dataaccess.entities.UserInterfaceEntity;

/**
 *
 * @author Loren Chorley
 */
@Stateful
public class ConcreteUserInterfaceFunctions extends AbstractBasicEntity<UserInterface, Long> implements UserInterfaceFunctions {
    private static final Logger logger = Logger.getLogger("spockdataaccess.ejb.requestsupport.UserInterfaceFunctions");
    
    public ConcreteUserInterfaceFunctions() {
    }
    
    @Override
    protected UserInterface newEntity(Long id) {
        try {
            UserInterface x = new UserInterfaceEntity();
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
            return "UserInterfaceEntity";
        } catch (Exception ex) {
            throw new EJBException("copyEntityProperties threw: " + ex.getMessage());
        }
    }

    @Override
    protected Class getEntityClass() {
        try {
            return UserInterfaceEntity.class;
        } catch (Exception ex) {
            throw new EJBException("copyEntityProperties threw: " + ex.getMessage());
        }
    }
    
    @Override
    protected void verifyBusinessLogic(UserInterface entity) {
        
    }
    
}
