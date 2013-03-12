package spockdataaccess.ejb.requestsupport;

import spockdataaccess.ejb.requestsupport.generalisations.BasicEntity;
import javax.persistence.EntityManager;
import spockdataaccess.entity.Configuration;

/**
 *
 * @author Loren Chorley
 */
public class ConfigurationFunctions extends BasicEntity<Configuration, String> {
    
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

    @Override
    protected void verifyBusinessLogic(Configuration entity) {
        
    }
    
}
