package spockdataaccess.ejb.requestsupport;

import javax.persistence.EntityManager;
import spockdataaccess.entity.InterfaceConnection;

/**
 * Reponsible for managing connections between environment and network interfaces
 * @author Loren Chorley
 */
public class ConnectionFunctions extends BasicEntityFunctions<InterfaceConnection, Long> {
    
    public static final int NETWORK_INTERFACE = 0;
    public static final int ENVIRONMENT_INTERFACE = 1;
    
    private EntityManager em;
    
    public ConnectionFunctions(EntityManager em) {
        super(em);
        this.em = em;
    }
    
    @Override
    protected InterfaceConnection newEntity(Long id) {
        InterfaceConnection x = new InterfaceConnection();
        x.setId(id);
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
    
}
