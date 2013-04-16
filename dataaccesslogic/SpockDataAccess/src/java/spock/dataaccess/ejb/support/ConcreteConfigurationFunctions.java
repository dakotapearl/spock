package spock.dataaccess.ejb.support;

import javax.persistence.EntityManager;
import spock.dataaccess.ejb.interfaces.ConfigurationFunctions;
import spock.dataaccess.ejb.interfaces.entities.Configuration;
import spock.dataaccess.entities.ConfigurationEntity;

/**
 *
 * @author Loren Chorley
 */
public class ConcreteConfigurationFunctions extends AbstractBasicEntity<Configuration, String> implements ConfigurationFunctions {
    
    public ConcreteConfigurationFunctions(EntityManager em) {
        super(em);
        this.em = em;
    }
       
    @Override
    protected Configuration newEntity(String id) {
        Configuration x = new ConfigurationEntity();
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
        return "ConfigurationEntity";
    }

    @Override
    protected Class getEntityClass() {
        return ConfigurationEntity.class;
    }

    @Override
    protected void verifyBusinessLogic(Configuration entity) {
        
    }
    
}
