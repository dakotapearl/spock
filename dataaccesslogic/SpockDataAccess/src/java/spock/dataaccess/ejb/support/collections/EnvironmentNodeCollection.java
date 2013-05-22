package spock.dataaccess.ejb.support.collections;

import java.util.Collection;
import javax.persistence.EntityManager;
import spock.dataaccess.ejb.interfaces.entities.Environment;
import spock.dataaccess.ejb.interfaces.entities.EnvironmentNode;
import spock.dataaccess.ejb.support.AbstractBasicEntityCollection;
import spock.dataaccess.entities.EnvironmentNodeEntity;
import javax.ejb.Stateful;

/**
 *
 * @author Loren Chorley
 */
//@Stateful
public class EnvironmentNodeCollection extends AbstractBasicEntityCollection<Environment, EnvironmentNode, Long> {

    public EnvironmentNodeCollection() {
    }

    @Override
    protected Collection<EnvironmentNode> getCollection(Environment container) {
        return container.getEnvironmentNodes();
    }

    @Override
    protected EnvironmentNode newCollectionEntity(Long id) {
        EnvironmentNode x = new EnvironmentNodeEntity();
        //x.setId(id);
        return x;
    }

    @Override
    protected void copyCollectionEntityProperties(EnvironmentNode sourceEntity, EnvironmentNode targetEntity) {
        targetEntity.setEnvironment(sourceEntity.getEnvironment());
        targetEntity.setEnvironmentInterface(sourceEntity.getEnvironmentInterface());
    }

    @Override
    protected Long getCollectionEntityID(EnvironmentNode entity) {
        return entity.getId();
    }

    @Override
    protected Class getCollectionEntityClass() {
        return EnvironmentNodeEntity.class;
    }

    @Override
    protected void verifyBusinessLogic(EnvironmentNode entity) {
        
    }

}
