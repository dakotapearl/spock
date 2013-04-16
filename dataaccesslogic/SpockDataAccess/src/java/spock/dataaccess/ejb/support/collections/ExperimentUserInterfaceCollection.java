package spock.dataaccess.ejb.support.collections;

import java.util.Collection;
import javax.persistence.EntityManager;
import spock.dataaccess.ejb.interfaces.entities.Experiment;
import spock.dataaccess.ejb.interfaces.entities.UserInterface;
import spock.dataaccess.ejb.support.AbstractBasicEntityCollection;
import spock.dataaccess.entities.UserInterfaceEntity;

/**
 *
 * @author Loren Chorley
 */
public class ExperimentUserInterfaceCollection extends AbstractBasicEntityCollection<Experiment, UserInterface, Long> {

    public ExperimentUserInterfaceCollection(EntityManager em) {
        super(em);
    }

    @Override
    protected Collection<UserInterface> getCollection(Experiment container) {
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
        targetEntity.setUser(sourceEntity.getUser());
        targetEntity.setExperiment(sourceEntity.getExperiment());
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
