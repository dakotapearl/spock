package spockdataaccess.ejb.requestsupport.entitycollections;

import java.util.Collection;
import javax.persistence.EntityManager;
import spockdataaccess.ejb.requestsupport.generalisations.BasicEntityCollection;
import spockdataaccess.entity.Environment;
import spockdataaccess.entity.EnvironmentNode;

/**
 *
 * @author Loren Chorley
 */
public class EnvironmentNodeCollection extends BasicEntityCollection<Environment, EnvironmentNode, Long> {

    public EnvironmentNodeCollection(EntityManager em) {
        super(em);
    }

    @Override
    protected Collection<EnvironmentNode> getCollection(Environment container) {
        return container.getEnvironmentNodes();
    }

    @Override
    protected EnvironmentNode newCollectionEntity(Long id) {
        EnvironmentNode x = new EnvironmentNode();
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
        return EnvironmentNode.class;
    }

    @Override
    protected void verifyBusinessLogic(EnvironmentNode entity) {
        
    }

}
