package spock.dataaccess.ejb.support;

import javax.ejb.EJBException;
import javax.persistence.EntityManager;
import spock.dataaccess.ejb.interfaces.ConnectionFunctions;
import spock.dataaccess.ejb.interfaces.entities.InterfaceConnection;
import spock.dataaccess.entities.InterfaceConnectionEntity;
import javax.ejb.Stateful;

/**
 * Reponsible for managing connections between environment and network interfaces
 * @author Loren Chorley
 */
@Stateful
public class ConcreteConnectionFunctions extends AbstractBasicEntity<InterfaceConnection, Long> implements ConnectionFunctions {
    
    public static final int NETWORK_INTERFACE = 0;
    public static final int ENVIRONMENT_INTERFACE = 1;
    
    public ConcreteConnectionFunctions() {
    }
    
    @Override
    protected InterfaceConnection newEntity(Long id) {
        InterfaceConnection x = new InterfaceConnectionEntity();
        //x.setId(id);
        return x;
    }

    @Override
    protected void copyEntityProperties(InterfaceConnection sourceEntity, InterfaceConnection targetEntity) {
        targetEntity.setEnvironmentInterface(sourceEntity.getEnvironmentInterface());
        targetEntity.setExperiment(sourceEntity.getExperiment());
        targetEntity.setNetworkInterface(sourceEntity.getNetworkInterface());
    }

    @Override
    protected Long getEntityID(InterfaceConnection entity) {
        return entity.getId();
    }

    @Override
    protected String getEntityName() {
        return "InterfaceConnectionEntity";
    }

    @Override
    protected Class getEntityClass() {
        return InterfaceConnectionEntity.class;
    }

    @Override
    protected void verifyBusinessLogic(InterfaceConnection entity) {
        if (entity.getNetworkInterface().getIsInputInterface() == entity.getEnvironmentInterface().getIsInputInterface()) {
            throw new EJBException("Business logic of InterfaceConnection violated: interfaces are incompatible. There must be one of each type in an interface connection.");
        }
    }
    
}
