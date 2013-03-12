package spockdataaccess.ejb.requestsupport.entitycollections;

import java.util.Collection;
import javax.persistence.EntityManager;
import spockdataaccess.ejb.requestsupport.generalisations.BasicEntityCollection;
import spockdataaccess.entity.Environment;
import spockdataaccess.entity.EnvironmentInterface;

/**
 *
 * @author Loren Chorley
 */
public class EnvironmentInterfaceCollection extends BasicEntityCollection<Environment, EnvironmentInterface, Long> {

    public EnvironmentInterfaceCollection(EntityManager em) {
        super(em);
    }

    @Override
    protected Collection<EnvironmentInterface> getCollection(Environment container) {
        return container.getEnvironmentInterfaces();
    }

    @Override
    protected EnvironmentInterface newCollectionEntity(Long id) {
        EnvironmentInterface x = new EnvironmentInterface();
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
        return EnvironmentInterface.class;
    }

    @Override
    protected void verifyBusinessLogic(EnvironmentInterface entity) {
        
    }

}
