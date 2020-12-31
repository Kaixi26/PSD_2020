package helloworld;

import helloworld.resources.DistrictResource;
import io.dropwizard.Application;
import io.dropwizard.setup.Bootstrap;
import io.dropwizard.setup.Environment;

import helloworld.resources.HelloWorldResource;
import helloworld.health.TemplateHealthCheck;

public class DiretorioApplication extends Application<DiretorioConfiguration> {

    public static void main(final String[] args) throws Exception {
        for(String s: args)
            System.out.println(s + "!");
        new DiretorioApplication().run(args);
    }

    @Override
    public String getName() {
        return "HelloWorld";
    }

    @Override
    public void initialize(final Bootstrap<DiretorioConfiguration> bootstrap) {
    }

    @Override
    public void run(final DiretorioConfiguration configuration,
                    final Environment environment) {
        environment.jersey().register(
            new HelloWorldResource(configuration.template, configuration.defaultName));
        environment.healthChecks().register("template",
            new TemplateHealthCheck(configuration.template));

        environment.jersey().register(
                new DistrictResource()
        );

    }

}
