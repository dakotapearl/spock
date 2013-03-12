package spockdataaccess.ejb.requestsupport;

import javax.ejb.EJBException;
import spockdataaccess.ejb.requestsupport.generalisations.BasicEntity;
import javax.persistence.EntityManager;
import spockdataaccess.entity.InterfaceConnection;

/**
 * Reponsible for managing connections between environment and network interfaces
 * @author Loren Chorley
 */
public class ConnectionFunctions extends BasicEntity<InterfaceConnection, Long> {
    
    public static final int NETWORK_INTERFACE = 0;
    public static final int ENVIRONMENT_INTERFACE = 1;
    
    public ConnectionFunctions(EntityManager em) {
        super(em);
        this.em = em;
    }
    
    @Override
    protected InterfaceConnection newEntity(Long id) {
        InterfaceConnection x = new InterfaceConnection();
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
        return "InterfaceConnection";
    }

    @Override
    protected Class getEntityClass() {
        return InterfaceConnection.class;
    }

    @Override
    protected void verifyBusinessLogic(InterfaceConnection entity) {
        if (entity.getNetworkInterface().getIsInputInterface() == entity.getEnvironmentInterface().getIsInputInterface()) {
            throw new EJBException("Business logic of InterfaceConnection violated: interfaces are incompatible. There must be one of each type in an interface connection.");
        }
    }
    
}
