package spock.dataaccess.ejb.support.collections;

import java.util.Collection;
import javax.persistence.EntityManager;
import spock.dataaccess.ejb.interfaces.entities.User;
import spock.dataaccess.ejb.interfaces.entities.UserInterface;
import spock.dataaccess.ejb.support.AbstractBasicEntityCollection;
import spock.dataaccess.entities.UserInterfaceEntity;

/**
 *
 * @author Loren Chorley
 */
public class UserUserInterfaceCollection extends AbstractBasicEntityCollection<User, UserInterface, Long> {

    public UserUserInterfaceCollection(EntityManager em) {
        super(em);
    }

    @Override
    protected Collection<UserInterface> getCollection(User container) {
        return container.getInterfaces();
    }

    @Override
    protected UserInterface newCollectionEntity(Long id) {
        UserInterface x = new UserInterfaceEntity();
        //x.setId(id);
        return x;
    }

    @Override
    protected void copyCollectionEntityProperties(UserInterface sourceEntity, UserInterface targetEntity) {
        targetEntity.setType(sourceEntity.getType());
        targetEntity.setIPAddress(sourceEntity.getIPAddress());
        targetEntity.setExperiment(sourceEntity.getExperiment());
        targetEntity.setUser(sourceEntity.getUser());
    }

    @Override
    protected Long getCollectionEntityID(UserInterface entity) {
        return entity.getId();
    }

    @Override
    protected Class getCollectionEntityClass() {
        return UserInterfaceEntity.class;
    }

    @Override
    protected void verifyBusinessLogic(UserInterface entity) {
        
    }

}
