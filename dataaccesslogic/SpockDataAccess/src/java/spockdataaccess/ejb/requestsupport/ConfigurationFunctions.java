package spockdataaccess.ejb.requestsupport;

import java.util.logging.Logger;
import javax.persistence.EntityManager;
import spockdataaccess.entity.Configuration;

/**
 *
 * @author Loren Chorley
 */
public class ConfigurationFunctions extends BasicEntityFunctions<Configuration, String> {
    
    private EntityManager em;
    
    public ConfigurationFunctions(EntityManager em) {
        super(em);
        this.em = em;
    }
       
    @Override
    protected Configuration newEntity(String id) {
        Configuration x = new Configuration();
        x.setName(id);
        return x;
    }

    @Override
    protected void copyEntityProperties(Configuration sourceEntity, Configuration targetEntity) {
        targetEntity.setConfigValue(sourceEntity.getConfigValue());
    }

    @Override
    protected String getEntityID(Configuration entity) {
        return entity.getName();
    }

    @Override
    protected String getEntityName() {
        return "Configuration";
    }

    @Override
    protected Class getEntityClass() {
        return Configuration.class;
    }
    
}
