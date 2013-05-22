package spock.dataaccess.ejb.support;

import java.util.logging.Logger;
import javax.persistence.EntityManager;
import spock.dataaccess.ejb.interfaces.MetricFunctions;
import spock.dataaccess.ejb.interfaces.entities.Metric;
import spock.dataaccess.entities.MetricEntity;
import javax.ejb.Stateful;

/**
 *
 * @author Loren Chorley
 */
@Stateful
public class ConcreteMetricFunctions extends AbstractBasicEntity<Metric, String> implements MetricFunctions {
    private static final Logger logger = Logger.getLogger("spockdataaccess.ejb.requestsupport.MetricFunctions");
    
    public ConcreteMetricFunctions() {
    }
    
    @Override
    protected Metric newEntity(String id) {
        Metric x = new MetricEntity();
        x.setId(id);
        return x;
    }

    @Override
    protected void copyEntityProperties(Metric sourceEntity, Metric targetEntity) {
        targetEntity.setCodeURL(sourceEntity.getCodeURL());
    }

    @Override
    protected String getEntityID(Metric entity) {
        return entity.getId();
    }

    @Override
    protected String getEntityName() {
        return "MetricEntity";
    }

    @Override
    protected Class getEntityClass() {
        return MetricEntity.class;
    }

    @Override
    protected void verifyBusinessLogic(Metric entity) {
        
    }
    
}
