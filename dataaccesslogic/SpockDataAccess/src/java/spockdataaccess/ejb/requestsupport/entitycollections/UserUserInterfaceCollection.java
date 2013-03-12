package spockdataaccess.ejb.requestsupport.entitycollections;

import java.util.Collection;
import javax.persistence.EntityManager;
import spockdataaccess.ejb.requestsupport.generalisations.BasicEntityCollection;
import spockdataaccess.entity.User;
import spockdataaccess.entity.UserInterface;

/**
 *
 * @author Loren Chorley
 */
public class UserUserInterfaceCollection extends BasicEntityCollection<User, UserInterface, Long> {

    public UserUserInterfaceCollection(EntityManager em) {
        super(em);
    }

    @Override
    protected Collection<UserInterface> getCollection(User container) {
        return container.getInterfaces();
    }

    @Override
    protected UserInterface newCollectionEntity(Long id) {
        UserInterface x = new UserInterface();
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
        return UserInterface.class;
    }

    @Override
    protected void verifyBusinessLogic(UserInterface entity) {
        
    }

}
