package spock.dataaccess.ejb.support.collections;

import java.util.Collection;
import javax.persistence.EntityManager;
import spock.dataaccess.ejb.interfaces.entities.Environment;
import spock.dataaccess.ejb.interfaces.entities.EnvironmentInterface;
import spock.dataaccess.ejb.support.AbstractBasicEntityCollection;
import spock.dataaccess.entities.EnvironmentInterfaceEntity;
import javax.ejb.Stateful;

/**
 *
 * @author Loren Chorley
 */
@Stateful
public class EnvironmentInterfaceCollection extends AbstractBasicEntityCollection<Environment, EnvironmentInterface, Long> {

    public EnvironmentInterfaceCollection() {
    }

    @Override
    protected Collection<EnvironmentInterface> getCollection(Environment container) {
        return container.getEnvironmentInterfaces();
    }

    @Override
    protected EnvironmentInterface newCollectionEntity(Long id) {
        EnvironmentInterface x = new EnvironmentInterfaceEntity();
        //x.setId(id);
        return x;
    }

    @Override
    protected void copyCollectionEntityProperties(EnvironmentInterface sourceEntity, EnvironmentInterface targetEntity) {
        targetEntity.setNumberOfNodes(sourceEntity.getNumberOfNodes());
        targetEntity.getEnvironmentNodes().clear();
        targetEntity.getEnvironmentNodes().addAll(sourceEntity.getEnvironmentNodes());
        targetEntity.setIsInputInterface(sourceEntity.getIsInputInterface());
        targetEntity.setEnvironment(sourceEntity.getEnvironment());
    }

    @Override
    protected Long getCollectionEntityID(EnvironmentInterface entity) {
        return entity.getId();
    }

    @Override
    protected Class getCollectionEntityClass() {
        return EnvironmentInterfaceEntity.class;
    }

    @Override
    protected void verifyBusinessLogic(EnvironmentInterface entity) {
        
    }

}
